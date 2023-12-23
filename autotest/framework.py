import os
import shutil
import time
from pathlib import Path
from subprocess import PIPE, STDOUT, Popen
from traceback import format_exc
from typing import Callable, Iterable, Iterator, List, Optional, Tuple, Union
from warnings import warn

import flopy
import numpy as np
from flopy.utils.compare import compare_heads
from modflow_devtools.executables import Executables
from modflow_devtools.misc import get_ostag, is_in_ci

from common_regression import (get_mf6_comparison, get_mf6_files,
                               get_namefiles, get_regression_files, setup_mf6,
                               setup_mf6_comparison)

DNODATA = 3.0e30
EXTTEXT = {
    "hds": "head",
    "hed": "head",
    "bhd": "head",
    "ucn": "concentration",
    "cbc": "cell-by-cell",
}


def adjust_htol(
    workspace: Union[str, os.PathLike], htol: float = 0.001
) -> Optional[float]:
    """Get outer_dvclose value from MODFLOW 6 ims file"""

    dvclose = get_dvclose(workspace)
    if not dvclose:
        return htol

    # adjust htol if < IMS outer_dvclose
    dvclose *= 5.0
    return dvclose if (htol is None or htol < dvclose) else htol


def api_return(success, model_ws) -> Tuple[bool, List[str]]:
    """
    parse libmf6 stdout shared object file
    """
    fpth = os.path.join(model_ws, "mfsim.stdout")
    return success, open(fpth).readlines()


def get_matching_files(
    workspace: Union[str, os.PathLike], extensions: Union[str, Iterator[str]]
) -> Iterator[str]:
    """
    Get MF6 regression files in the specified workspace,
    optionally filtering by one or more file extensions.
    Parameters
    ----------
    workspace : str or PathLike
        MODFLOW 6 simulation workspace path
    extensions : str or list of str
        file extensions to filter
    Returns
    -------
    An iterator of regression files found
    """

    workspace = Path(workspace).expanduser().absolute()
    if isinstance(extensions, str):
        extensions = [extensions]

    for ext in extensions:
        for file in workspace.glob(f"*.{ext}"):
            yield file


def get_dvclose(workspace: Union[str, os.PathLike]) -> Optional[float]:
    """Get outer_dvclose value from MODFLOW 6 ims file"""
    dvclose = None
    files = os.listdir(workspace)
    for file_name in files:
        pth = os.path.join(workspace, file_name)
        if os.path.isfile(pth):
            if file_name.lower().endswith(".ims"):
                with open(pth) as f:
                    lines = f.read().splitlines()
                for line in lines:
                    if "outer_dvclose" in line.lower():
                        v = float(line.split()[1])
                        if dvclose is None:
                            dvclose = v
                        else:
                            if v > dvclose:
                                dvclose = v
                        break

    return dvclose


def get_rclose(workspace: Union[str, os.PathLike]) -> Optional[float]:
    """Get inner_rclose value from MODFLOW 6 ims file"""

    rclose = None
    for pth in workspace.glob("*.ims"):
        with open(pth, "r") as f:
            for line in f:
                if "inner_rclose" in line.lower():
                    v = float(line.split()[1])
                    if rclose is None:
                        rclose = v
                    else:
                        if v > rclose:
                            rclose = v
                    break

    if rclose is None:
        return 0.5

    rclose *= 5.0
    return rclose


def get_mfsim_lst_tail(path: os.PathLike, lines=100) -> str:
    """Get the tail of the mfsim.lst listing file"""
    msg = ""
    _lines = open(path).read().splitlines()
    msg = "\n" + 79 * "-" + "\n"
    i0 = -lines if len(_lines) > lines else 0
    for line in _lines[i0:]:
        if len(line) > 0:
            msg += f"{line}\n"
    msg += 79 * "-" + "\n\n"
    return msg


def run_parallel(workspace, target, ncpus) -> Tuple[bool, List[str]]:
    if not is_in_ci() and get_ostag() in ["mac"]:
        oversubscribed = ["--hostfile", "localhost"]
        with open(f"{workspace}/localhost", "w") as f:
            f.write(f"localhost slots={ncpus}\n")
    else:
        oversubscribed = ["--oversubscribe"]

    normal_msg = "normal termination"
    success = False
    nr_success = 0
    buff = []

    # parallel commands
    mpiexec_cmd = (
        ["mpiexec"] + oversubscribed + ["-np", str(ncpus), target, "-p"]
    )

    proc = Popen(mpiexec_cmd, stdout=PIPE, stderr=STDOUT, cwd=workspace)

    while True:
        line = proc.stdout.readline().decode("utf-8")
        if line == "" and proc.poll() is not None:
            break
        if line:
            # success is when the success message appears
            # in every process of the parallel simulation
            if normal_msg in line.lower():
                nr_success += 1
                if nr_success == ncpus:
                    success = True
            line = line.rstrip("\r\n")
            print(line)
            buff.append(line)
        else:
            break

    return success, buff


def write_input(*sims, verbose=True):
    """
    Write input files for `flopy.mf6.MFSimulation` or `flopy.mbase.BaseModel`.

    Parameters
    ----------

    sims : arbitrary list
        Simulations or models
    verbose : bool, optional
        whether to show verbose output
    """

    if sims is None:
        warn("No simulations or models!")
        return

    # write input files for each model or simulation
    for sim in sims:
        if sim is None:
            continue

        if isinstance(sim, flopy.mf6.MFSimulation):
            if verbose:
                print(
                    f"Writing mf6 simulation '{sim.name}' to: {sim.sim_path}"
                )
            sim.write_simulation()
        elif isinstance(sim, flopy.mbase.BaseModel):
            if verbose:
                print(
                    f"Writing {type(sim)} model '{sim.name}' to: {sim.model_ws}"
                )
            sim.write_input()
        else:
            raise ValueError(f"Unsupported simulation/model type: {type(sim)}")


class TestFramework:
    """
    Defines a MODFLOW 6 test and its lifecycle, with configurable
    hooks to evaluate results or run other models for comparison:

        - MODFLOW-2005
        - MODFLOW-NWT
        - MODFLOW-USG
        - MODFLOW-LGR

    Parameters
    ----------
    name : str
        The test name
    workspace : pathlike
        The test workspace
    targets : Executables
        Binary targets to test against. Development binaries are
        required, downloads/rebuilt binaries are optional (if not
        found, comparisons and regression tests will be skipped).
    build : function, optional
        User defined function returning one or more simulations/models.
        Takes `self` as input. This is the place to build simulations.
        The first simulation is taken to be the reference, any further
        simulations/models are for comparison. If no build function is
        provided, input files must be written before calling `run()`.
    check : function, optional
        User defined function to evaluate results of the simulation.
        Takes `self` as input. This is a good place for assertions.
    parallel : bool, optional
        Whether to test mf6 parallel capabilities.
    ncpus : int, optional
        Number of CPUs for mf6 parallel testing.
    htol : float, optional
        Tolerance for result comparisons.
    rclose : float, optional
        Residual tolerance for convergence
    verbose: bool, optional
        Whether to show verbose output
    xfail : bool, optional
        Whether the test is expected to fail
    api_func: function, optional
        User defined function invoking the MODFLOW API, accepting
        the MF6 library path and the test workspace as parameters
    compare: str, optional
        String selecting the comparison type to perform: 'auto',
        'mf6_regression', or 'run_only'
    """

    # tell pytest this class doesn't contain tests, don't collect it
    __test__ = False

    def __init__(
        self,
        name: str,
        workspace: Union[str, os.PathLike],
        targets: Union[dict, Executables],
        api_func: Optional[Callable] = None,
        build: Optional[Callable] = None,
        check: Optional[Callable] = None,
        compare: Optional[str] = "auto",
        parallel=False,
        ncpus=1,
        htol=None,
        rclose=None,
        verbose=False,
        xfail=False,
    ):
        # make sure workspace exists
        workspace = Path(workspace).expanduser().absolute()
        assert workspace.is_dir(), f"{workspace} is not a valid directory"
        if verbose:
            from pprint import pprint

            print("Initializing test", name, "in workspace", workspace)
            contents = list(workspace.glob("*"))
            if any(contents):
                print(f"Workspace is not empty:")
                pprint(contents)

        self.name = name
        self.workspace = workspace
        self.targets = (
            Executables(**targets) if isinstance(targets, dict) else targets
        )
        self.build = build
        self.check = check
        self.parallel = parallel
        self.ncpus = ncpus
        self.api_func = api_func
        self.compare = compare
        self.inpt = None
        self.outp = None
        self.coutp = None
        self.htol = 0.001 if htol is None else htol
        self.rclose = 0.001 if rclose is None else rclose
        self.verbose = verbose
        self.xfail = xfail

    def __repr__(self):
        return self.name

    # private

    def _compare_heads(self, cpth=None, extensions="hds") -> bool:
        if isinstance(extensions, str):
            extensions = [extensions]

        if cpth:
            files1 = []
            files2 = []
            exfiles = []
            for file1 in self.outp:
                ext = os.path.splitext(file1)[1][1:]

                if ext.lower() in extensions:
                    # simulation file
                    pth = os.path.join(self.workspace, file1)
                    files1.append(pth)

                    # look for an exclusion file
                    pth = os.path.join(self.workspace, file1 + ".ex")
                    if os.path.isfile(pth):
                        exfiles.append(pth)
                    else:
                        exfiles.append(None)

                    # Check to see if there is a corresponding compare file
                    if self.coutp is not None:
                        for file2 in self.coutp:
                            ext = os.path.splitext(file2)[1][1:]
                            if ext.lower() in extensions:
                                # simulation file
                                pth = os.path.join(cpth, file2)
                                files2.append(pth)
                    else:
                        files2.append(None)

            if self.cmp_namefile is None:
                pth = None
            else:
                pth = os.path.join(cpth, self.cmp_namefile)

            for i in range(len(files1)):
                file1 = files1[i]
                ext = os.path.splitext(file1)[1][1:].lower()
                outfile = os.path.splitext(os.path.basename(file1))[0]
                outfile = os.path.join(
                    self.workspace, outfile + "." + ext + ".cmp.out"
                )
                file2 = None if files2 is None else files2[i]

                # set exfile
                exfile = None
                if file2 is None:
                    if len(exfiles) > 0:
                        exfile = exfiles[i]
                        if exfile is not None:
                            print(
                                f"Exclusion file {i + 1}",
                                os.path.basename(exfile),
                            )

                # make comparison
                success = compare_heads(
                    None,
                    pth,
                    precision="double",
                    text=EXTTEXT[ext],
                    outfile=outfile,
                    files1=file1,
                    files2=file2,
                    htol=self.htol,
                    difftol=True,
                    verbose=self.verbose,
                    exfile=exfile,
                )
                print(f"{EXTTEXT[ext]} comparison {i + 1}", self.name)
                if not success:
                    return False
            return True

        # otherwise it's a regression comparison
        files0, files1 = get_regression_files(self.workspace, extensions)
        extension = "hds"
        for i, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.workspace, outfile + f".{extension}.cmp.out"
            )
            success = compare_heads(
                None,
                None,
                precision="double",
                htol=self.htol,
                text=EXTTEXT[extension],
                outfile=outfile,
                files1=fpth0,
                files2=fpth1,
                verbose=self.verbose,
            )
            print(
                (
                    f"{EXTTEXT[extension]} comparison {i + 1}"
                    + f"{self.name} ({os.path.basename(fpth0)})"
                )
            )
            if not success:
                return False
        return True

    def _compare_concentrations(self, extensions="ucn") -> bool:
        if isinstance(extensions, str):
            extensions = [extensions]

        files0, files1 = get_regression_files(self.workspace, extensions)
        extension = "ucn"
        for i, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.workspace, outfile + f".{extension}.cmp.out"
            )
            success = compare_heads(
                None,
                None,
                precision="double",
                htol=self.htol,
                text=EXTTEXT[extension],
                outfile=outfile,
                files1=fpth0,
                files2=fpth1,
                verbose=self.verbose,
            )
            print(
                (
                    f"{EXTTEXT[extension]} comparison {i + 1}"
                    + f"{self.name} ({os.path.basename(fpth0)})",
                )
            )
            if not success:
                return False
        return True

    def _compare_budgets(self, extensions="cbc") -> bool:
        if isinstance(extensions, str):
            extensions = [extensions]
        files0, files1 = get_regression_files(self.workspace, extensions)
        extension = "cbc"
        for i, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            print(
                f"{EXTTEXT[extension]} comparison {i + 1}",
                f"{self.name} ({os.path.basename(fpth0)})",
            )
            success = self._compare_budget_files(
                extension,
                fpth0,
                fpth1,
            )
            if not success:
                return False
        return True

    def _compare_budget_files(self, extension, fpth0, fpth1) -> bool:
        success = True
        if os.stat(fpth0).st_size * os.stat(fpth0).st_size == 0:
            return success, ""
        outfile = os.path.splitext(os.path.basename(fpth0))[0]
        outfile = os.path.join(
            self.workspace, outfile + f".{extension}.cmp.out"
        )
        fcmp = open(outfile, "w")
        fcmp.write("Performing CELL-BY-CELL to CELL-BY-CELL comparison\n")
        fcmp.write(f"{fpth0}\n")
        fcmp.write(f"{fpth1}\n\n")

        # open the files
        cbc0 = flopy.utils.CellBudgetFile(
            fpth0, precision="double", verbose=self.verbose
        )
        cbc1 = flopy.utils.CellBudgetFile(
            fpth1, precision="double", verbose=self.verbose
        )

        # build list of cbc data to retrieve
        avail0 = cbc0.get_unique_record_names()
        avail1 = cbc1.get_unique_record_names()
        avail0 = [t.decode().strip() for t in avail0]
        avail1 = [t.decode().strip() for t in avail1]

        # initialize list for storing totals for each budget term terms
        cbc_keys0 = []
        cbc_keys1 = []
        for t in avail0:
            t1 = t
            if t not in avail1:
                # check if RCHA or EVTA is available and use that instead
                # should be able to remove this once v6.3.0 is released
                if t[:-1] in avail1:
                    t1 = t[:-1]
                else:
                    raise Exception(f"Could not find {t} in {fpth1}")
            cbc_keys0.append(t)
            cbc_keys1.append(t1)

        # get list of times and kstpkper
        kk = cbc0.get_kstpkper()
        times = cbc0.get_times()

        # process data
        for key, key1 in zip(cbc_keys0, cbc_keys1):
            for idx, (k, t) in enumerate(zip(kk, times)):
                v0 = cbc0.get_data(kstpkper=k, text=key)[0]
                v1 = cbc1.get_data(kstpkper=k, text=key1)[0]
                if v0.dtype.names is not None:
                    v0 = v0["q"]
                    v1 = v1["q"]
                # skip empty vectors
                if v0.size < 1:
                    continue
                vmin = self.rclose
                if vmin < 1e-6:
                    vmin = 1e-6
                vmin_tol = 5.0 * vmin
                if v0.shape != v1.shape:
                    v0 = v0.flatten()
                    v1 = v1.flatten()
                idx = (abs(v0) > vmin) & (abs(v1) > vmin)
                diff = np.zeros(v0.shape, dtype=v0.dtype)
                diff[idx] = abs(v0[idx] - v1[idx])
                diffmax = diff.max()
                indices = np.where(diff == diffmax)[0]
                if diffmax > vmin_tol:
                    success = False
                    msg = (
                        f"{os.path.basename(fpth0)} - "
                        + f"{key:16s} "
                        + f"difference ({diffmax:10.4g}) "
                        + f"> {vmin_tol:10.4g} "
                        + f"at {indices.size} nodes "
                        + f" [first location ({indices[0] + 1})] "
                        + f"at time {t} "
                    )
                    fcmp.write(f"{msg}\n")
                    if self.verbose:
                        print(msg)

        fcmp.close()
        return success

    # public

    def setup(self, src, dst):
        print("Setting up MF6 test", self.name)
        print("  Source:", src)
        print("  Destination:", dst)
        self.workspace = dst

        # setup expected input and output files
        self.inpt, self.outp = setup_mf6(src=src, dst=dst)
        print("waiting...")
        time.sleep(0.5)

        # adjust htol if it is smaller than IMS outer_dvclose
        self.htol = adjust_htol(self.workspace)
        self.rclose = get_rclose(self.workspace)

        if self.compare == "mf6_regression":
            shutil.copytree(self.workspace, self.workspace / self.compare)
        else:
            # get the type of comparison to use
            self.compare = get_mf6_comparison(src)
            setup_mf6_comparison(src, dst, self.compare, overwrite=True)

    def run_sim_or_model(self, workspace, target="mf6", xfail=False) -> bool:
        """
        Run a simulation or model with FloPy.

        workspace : str or path-like
            The simulation or model workspace
        exe : str or path-like
            The executable to use
        """

        target = str(target)
        if not (target in self.targets.as_dict() or shutil.which(target)):
            raise ValueError(f"Target executable not found: {target}")

        # needed in _compare_heads()... todo: inject explicitly?
        self.cmp_namefile = (
            None
            if "mf6" in target
            or "libmf6" in target
            or "mf6_regression" in target
            else os.path.basename(get_namefiles(workspace)[0])
        )
        if self.verbose:
            print(f"Running {target} in {workspace}")

        # run the model
        try:
            # via MODFLOW API
            if "libmf6" in target and self.api_func:
                success, _ = self.api_func(target, workspace)
            # via MF6 executable
            elif "mf6" in target:
                # parallel test if configured
                if self.parallel:
                    print(
                        f"Parallel test {self.name} on {self.ncpus} processes"
                    )
                    try:
                        success, _ = run_parallel(
                            workspace, target, self.ncpus
                        )
                    except Exception:
                        warn(
                            "MODFLOW 6 parallel test",
                            self.name,
                            f"failed with error:\n{format_exc()}",
                        )
                        success = False
                else:
                    # otherwise serial run
                    try:
                        success, _ = flopy.run_model(
                            target,
                            self.workspace / "mfsim.nam",
                            model_ws=workspace,
                        )
                    except Exception:
                        warn(
                            "MODFLOW 6 serial test",
                            self.name,
                            f"failed with error:\n{format_exc()}",
                        )
                        success = False
            else:
                # non-MF6 model
                try:
                    success, _ = flopy.run_model(
                        target, self.cmp_namefile, workspace
                    )
                except Exception:
                    warn(f"{target} model failed:\n{format_exc()}")
                    success = False

            if xfail:
                if success:
                    warn("MODFLOW 6 model should have failed!")
                    success = False
                else:
                    success = True

            lst_file_path = Path(workspace) / "mfsim.lst"
            if "mf6" in target and not success and lst_file_path.is_file():
                warn(
                    "MODFLOW 6 listing file ended with: \n"
                    + get_mfsim_lst_tail(lst_file_path)
                )
        except Exception:
            success = False
            warn(
                f"Unhandled error in comparison model {self.name}:\n{format_exc()}"
            )

        return success

    def compare_output(self, compare="auto"):
        """
        Compare the main model's output with a reference or regression model's output.

        comparison : str
            The comparison type: "auto", "mf6", "mf6_regression", etc
        """

        if compare is None:
            raise ValueError(f"Comparison type not specified")
        elif compare == "run_only":
            raise ValueError(
                f"Comparison type 'run_only' specified, skipping comparison"
            )

        print("Comparison test", self.name)

        hds_ext = (
            "hds",
            "hed",
            "bhd",
            "ahd",
            "bin",
        )
        cbc_ext = (
            "cbc",
            "bud",
        )
        cmp_path = self.workspace / compare
        if "mf6" in compare:
            _, self.coutp = get_mf6_files(cmp_path / "mfsim.nam")
        if "mf6_regression" in compare:
            assert self._compare_heads(
                extensions=hds_ext
            ), "head comparison failed"
            assert self._compare_budgets(
                extensions=cbc_ext
            ), "budget comparison failed"
            assert (
                self._compare_concentrations()
            ), "concentration comparison failed"
        else:
            assert self._compare_heads(
                cpth=cmp_path, extensions=hds_ext
            ), "head comparison failed"

    def run(self):
        """
        Run the test case end-to-end.

        """

        # build models/simulations, store keyed by
        # workspace path, and write input files
        if self.build:
            sims = self.build(self)
            sims = sims if isinstance(sims, Iterable) else [sims]
            sims = [sim for sim in sims if sim]  # filter Nones
            self.sims = {
                (
                    sim.sim_path
                    if isinstance(sim, flopy.mf6.MFSimulation)
                    else sim.model_ws
                ): sim
                for sim in sims
            }
            write_input(*sims, verbose=self.verbose)

        # run main model(s) and get expected output files
        assert self.run_sim_or_model(
            self.workspace, self.targets.mf6, self.xfail
        ), f"MODFLOW 6 simulation failed: {self.workspace}"
        _, self.outp = get_mf6_files(
            self.workspace / "mfsim.nam", self.verbose
        )

        # setup and run comparison model(s), if enabled
        if self.compare:
            # adjust htol if < IMS outer_dvclose, and rclose for budget comparisons
            self.htol = adjust_htol(self.workspace, self.htol)
            self.rclose = get_rclose(self.workspace)

            # try to autodetect comparison type if enabled
            if self.compare == "auto":
                if self.verbose:
                    print("Auto-detecting comparison type")
                self.compare = get_mf6_comparison(self.workspace)
            if self.compare:
                if self.verbose:
                    print(f"Using comparison type: {self.compare}")

                # copy reference model files if mf6 regression
                if self.compare == "mf6_regression":
                    cmp_path = self.workspace / self.compare
                    if os.path.isdir(cmp_path):
                        if self.verbose:
                            print(f"Cleaning {cmp_path}")
                        shutil.rmtree(cmp_path)
                    if self.verbose:
                        print(
                            f"Copying reference model files from {self.workspace} to {cmp_path}"
                        )
                    shutil.copytree(self.workspace, cmp_path)

                # run comparison model(s) with the comparison executable, key can be
                #   - mf6
                #   - mf6_regression
                #   - libmf6
                #   - mf2005
                #   - mfnwt
                #   - mfusg
                #   - mflgr

                # todo: don't hardcode workspace & assume agreement with test case
                # simulation workspace, store & access workspaces directly
                run_only = (
                    self.compare == "run_only"
                )  # just run, don't compare results
                workspace = (
                    self.workspace / "mf6"
                    if run_only
                    else self.workspace / self.compare
                )
                assert self.run_sim_or_model(
                    workspace,
                    self.targets.get(self.compare, self.targets.mf6),
                ), f"Comparison model failed: {workspace}"

                # compare model results, if enabled
                if not run_only:
                    if self.verbose:
                        print("Comparing model outputs")
                    self.compare_output(self.compare)
            else:
                warn("Could not detect comparison type, aborting comparison")

        # check results, if enabled
        if self.check:
            if self.verbose:
                print("Checking model outputs against expectation")
            self.check(self)
