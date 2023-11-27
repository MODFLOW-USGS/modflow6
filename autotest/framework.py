import os
import shutil
import time
from subprocess import PIPE, STDOUT, Popen
from warnings import warn

import flopy
import numpy as np
from common_regression import (
    get_mf6_comparison,
    get_mf6_files,
    get_namefiles,
    setup_mf6,
    setup_mf6_comparison,
)
from flopy.utils.compare import compare_heads
from modflow_devtools.executables import Executables
from modflow_devtools.misc import get_ostag, is_in_ci

COMPARE_OPTIONS = [
    "compare",
    ".cmp",
    "mf2005",
    "mf2005.cmp",
    "mfnwt",
    "mfnwt.cmp",
    "mfusg",
    "mfusg.cmp",
    "mflgr",
    "mflgr.cmp",
    "libmf6",
    "libmf6.cmp",
    "mf6",
    "mf6.cmp",
]
DNODATA = 3.0e30
EXTS = {
    "hds": "head",
    "hed": "head",
    "bhd": "head",
    "ucn": "concentration",
    "cbc": "cell-by-cell",
}
SFMT = "{:25s} - {}"


def api_return(success, model_ws):
    """
    parse libmf6 stdout shared object file
    """
    fpth = os.path.join(model_ws, "mfsim.stdout")
    return success, open(fpth).readlines()


def get_dvclose(dir_pth):
    """Get outer_dvclose value from MODFLOW 6 ims file"""
    dvclose = None
    files = os.listdir(dir_pth)
    for file_name in files:
        pth = os.path.join(dir_pth, file_name)
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


def get_rclose(dir_pth):
    """Get inner_rclose value from MODFLOW 6 ims file"""
    rclose = None
    files = os.listdir(dir_pth)
    for file_name in files:
        pth = os.path.join(dir_pth, file_name)
        if os.path.isfile(pth):
            if file_name.lower().endswith(".ims"):
                with open(pth) as f:
                    lines = f.read().splitlines()
                for line in lines:
                    if "inner_rclose" in line.lower():
                        v = float(line.split()[1])
                        if rclose is None:
                            rclose = v
                        else:
                            if v > rclose:
                                rclose = v
                        break

    return rclose


def write_models(sims, verbose=True):
    """
    Write simulation/model input files.

    Parameters
    ----------

    sims : list or array-like
        simulations or models
    verbose : bool
        whether to show verbose output
    """

    if sims is None:
        raise ValueError(f"Simulations or models required")

    # make sure we have a list
    if not isinstance(sims, (tuple, list, np.ndarray)):
        sims = [sims]

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
    # tell pytest this class doesn't contain tests, don't collect it
    __test__ = False

    """
    Test harness for MODFLOW 6.

    Supports:
     * custom hooks to build models/simulations at test time
     * custom hooks to check model/simulation result correctness
     * testing some modflow6 extended features (currently: parallel)
     * comparing MODFLOW 6 with MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR

    Parameters
    ----------
    name : str
        The test case name
    workspace : pathlike
        The test workspace
    targets : Executables
        Binary targets to test against. Development binaries are
        required, downloads/rebuilt binaries are optional (if not
        found, comparisons and regression tests will be skipped).
    build : function, optional
        User defined function returning one or more simulations/models.
        The first simulation is taken to be the reference, any further
        simulations/models are for comparison. If no build function is
        provided the build step is skipped (in this case model input
        files must be written manually before `run()` is invoked).
    check : function, optional
        User defined function to evaluate results of the simulation.
        Takes `self` as input. This is a good place for assertions.
        If no check function is provided, evaluations are skipped.
    parallel : bool, optional
        Whether to test mf6 parallel capabilities.
    ncpus : int, optional
        Number of CPUs for mf6 parallel testing.
    htol : float
        Tolerance for result comparisons.
    """

    # builtins

    def __init__(
        self,
        name: str,
        workspace: os.PathLike,
        targets: Executables,
        build=None,
        check=None,
        parallel=False,
        ncpus=1,
        htol=None,
        pdtol=None,
        rclose=None,
        cmp_verbose=True,
        xfail=None,
        api_func=None,
        comparison="compare",
        make_comparison=True,
        run_comparison=None,
    ):
        msg = SFMT.format("Initializing test", name)
        print(msg)

        self.name = name
        self.workspace = workspace
        self.targets = targets
        self.build = build
        self.check = check
        self.parallel = parallel
        self.ncpus = ncpus
        self.api_func = api_func
        self.comparison = comparison
        self.make_comparison = make_comparison
        if run_comparison is None:
            self.run_comparison = make_comparison
        else:
            self.run_comparison = run_comparison

        self.inpt = None
        self.outp = None
        self.coutp = None

        # make sure workspace exists
        assert (
            self.workspace.is_dir()
        ), f"{self.workspace} is not a valid directory"

        # set htol for comparisons
        if htol is None:
            htol = 0.001
        else:
            msg = SFMT.format("User specified comparison htol", htol)
            print(msg)

        self.htol = htol

        # set pdtol for comparisons
        if pdtol is None:
            pdtol = 0.001
        else:
            msg = SFMT.format(
                "User specified percent difference comparison pdtol", pdtol
            )
            print(msg)

        self.pdtol = pdtol

        # set rclose for comparisons
        if rclose is None:
            rclose = 0.001
        else:
            msg = SFMT.format(
                "User specified percent difference comparison rclose", rclose
            )
            print(msg)

        self.rclose = rclose

        # set compare verbosity
        self.cmp_verbose = cmp_verbose

        # set allow failure
        self.xfail = xfail

        self.success = False

    def __repr__(self):
        return self.name

    # private

    def _run_parallel(self, exe):
        if not is_in_ci() and get_ostag() in ["mac"]:
            oversubscribed = ["--hostfile", "localhost"]
            with open(f"{self.workspace}/localhost", "w") as f:
                f.write(f"localhost slots={self.ncpus}\n")
        else:
            oversubscribed = ["--oversubscribe"]

        normal_msg = "normal termination"
        success = False
        nr_success = 0
        buff = []

        # parallel commands
        mpiexec_cmd = (
            ["mpiexec"] + oversubscribed + ["-np", str(self.ncpus), exe, "-p"]
        )

        proc = Popen(
            mpiexec_cmd, stdout=PIPE, stderr=STDOUT, cwd=self.workspace
        )

        while True:
            line = proc.stdout.readline().decode("utf-8")
            if line == "" and proc.poll() is not None:
                break
            if line:
                # success is when the success message appears
                # in every process of the parallel simulation
                if normal_msg in line.lower():
                    nr_success += 1
                    if nr_success == self.ncpus:
                        success = True
                line = line.rstrip("\r\n")
                print(line)
                buff.append(line)
            else:
                break

        return success, buff

    def _get_mfsim_listing(self, lst_pth):
        """Get the tail of the mfsim.lst listing file"""
        msg = ""
        ilen = 100
        with open(lst_pth) as fp:
            lines = fp.read().splitlines()
        msg = "\n" + 79 * "-" + "\n"
        if len(lines) > ilen:
            i0 = -100
        else:
            i0 = 0
        for line in lines[i0:]:
            if len(line) > 0:
                msg += f"{line}\n"
        msg += 79 * "-" + "\n\n"
        return msg

    def _regression_files(self, extensions):
        if isinstance(extensions, str):
            extensions = [extensions]
        files = os.listdir(self.workspace)
        files0 = []
        files1 = []
        for file_name in files:
            fpth0 = os.path.join(self.workspace, file_name)
            if os.path.isfile(fpth0):
                for extension in extensions:
                    if file_name.lower().endswith(extension):
                        files0.append(fpth0)
                        fpth1 = os.path.join(
                            self.workspace, "mf6_regression", file_name
                        )
                        files1.append(fpth1)
                        break
        return files0, files1

    def _compare_heads(self, comparison=None, cpth=None, extensions="hds"):
        if isinstance(extensions, str):
            extensions = [extensions]

        # if a comparison path is provided, compare with the reference model results
        if cpth:
            files_cmp = None
            if comparison is not None and comparison.lower() == "compare":
                files_cmp = []
                files = os.listdir(cpth)
                for file in files:
                    files_cmp.append(file)

            files1 = []
            files2 = []
            exfiles = []
            ipos = 0
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
                    if files_cmp is not None:
                        if file1 + ".cmp" in files_cmp:
                            # compare file
                            idx = files_cmp.index(file1 + ".cmp")
                            pth = os.path.join(cpth, files_cmp[idx])
                            files2.append(pth)
                            txt = SFMT.format(
                                f"Comparison file {ipos + 1}",
                                os.path.basename(pth),
                            )
                            print(txt)
                    else:
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

            for ipos in range(len(files1)):
                file1 = files1[ipos]
                ext = os.path.splitext(file1)[1][1:].lower()
                outfile = os.path.splitext(os.path.basename(file1))[0]
                outfile = os.path.join(
                    self.workspace, outfile + "." + ext + ".cmp.out"
                )
                if files2 is None:
                    file2 = None
                else:
                    file2 = files2[ipos]

                # set exfile
                exfile = None
                if file2 is None:
                    if len(exfiles) > 0:
                        exfile = exfiles[ipos]
                        if exfile is not None:
                            txt = SFMT.format(
                                f"Exclusion file {ipos + 1}",
                                os.path.basename(exfile),
                            )
                            print(txt)

                # make comparison
                success = compare_heads(
                    None,
                    pth,
                    precision="double",
                    text=EXTS[ext],
                    outfile=outfile,
                    files1=file1,
                    files2=file2,
                    htol=self.htol,
                    difftol=True,
                    # Change to true to have list of all nodes exceeding htol
                    verbose=self.cmp_verbose,
                    exfile=exfile,
                )
                msg = SFMT.format(
                    f"{EXTS[ext]} comparison {ipos + 1}",
                    self.name,
                )
                print(msg)

                if not success:
                    break

            return True

        # otherwise it's a regression comparison
        files0, files1 = self._regression_files(extensions)
        extension = "hds"
        ipos = 0
        for idx, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.workspace, outfile + f".{extension}.cmp.out"
            )
            success = compare_heads(
                None,
                None,
                precision="double",
                htol=self.htol,
                text=EXTS[extension],
                outfile=outfile,
                files1=fpth0,
                files2=fpth1,
                verbose=self.cmp_verbose,
            )
            msg = SFMT.format(
                f"{EXTS[extension]} comparison {ipos + 1}",
                f"{self.name} ({os.path.basename(fpth0)})",
            )
            ipos += 1
            print(msg)

            if not success:
                break

        return success

    def _compare_concentrations(self, extensions="ucn"):
        if isinstance(extensions, str):
            extensions = [extensions]
        success = True
        files0, files1 = self._regression_files(extensions)
        extension = "ucn"
        ipos = 0
        for fpth0, fpth1 in zip(files0, files1):
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.workspace, outfile + f".{extension}.cmp.out"
            )
            success = compare_heads(
                None,
                None,
                precision="double",
                htol=self.htol,
                text=EXTS[extension],
                outfile=outfile,
                files1=fpth0,
                files2=fpth1,
                verbose=self.cmp_verbose,
            )
            msg = SFMT.format(
                f"{EXTS[extension]} comparison {ipos + 1}",
                f"{self.name} ({os.path.basename(fpth0)})",
            )
            ipos += 1
            print(msg)

            if not success:
                break

        return success

    def _compare_budgets(self, extensions="cbc"):
        if isinstance(extensions, str):
            extensions = [extensions]
        success = True
        files0, files1 = self._regression_files(extensions)
        extension = "cbc"
        ipos = 0
        for idx, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            success, msg = self._compare_budget_files(
                ipos,
                extension,
                fpth0,
                fpth1,
            )
            ipos += 1

            if not success:
                break

        return success

    def _compare_budget_files(self, ipos, extension, fpth0, fpth1):
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
            fpth0, precision="double", verbose=self.cmp_verbose
        )
        cbc1 = flopy.utils.CellBudgetFile(
            fpth1, precision="double", verbose=self.cmp_verbose
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
        success_tst = True
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
                    if self.cmp_verbose:
                        print(msg)

        msg = SFMT.format(
            f"{EXTS[extension]} comparison {ipos + 1}",
            f"{self.name} ({os.path.basename(fpth0)})",
        )
        print(msg)
        fcmp.close()

        return success, msg

    # public

    def setup(self, src, dst):
        print(
            SFMT.format(
                "Setting up MF6 test", self.name, "from existing model"
            )
        )
        self.workspace = dst
        print(f"source:      {src}")
        print(f"destination: {dst}")

        # setup expected input and output files
        self.inpt, self.outp = setup_mf6(src=src, dst=dst)
        print("waiting...")
        time.sleep(0.5)

        # adjust htol if it is smaller than IMS outer_dvclose
        dvclose = get_dvclose(dst)
        if dvclose is not None:
            dvclose *= 5.0
            if self.htol < dvclose:
                self.htol = dvclose

        # get rclose to use with budget comparisons
        rclose = get_rclose(dst)
        if rclose is None:
            rclose = 0.5
        else:
            rclose *= 5.0
        self.rclose = rclose

        if self.comparison == "mf6_regression":
            pth = os.path.join(dst, self.comparison)
            if os.path.isdir(pth):
                shutil.rmtree(pth)
            shutil.copytree(dst, pth)
        else:
            # get the type of comparison to use
            self.comparison = get_mf6_comparison(src)
            setup_mf6_comparison(src, dst, self.comparison, overwrite=True)

    def run_main_model(self):
        """
        Run primary model(s).
        """

        exe = str(self.targets["mf6"].absolute())
        nam_file = self.workspace / "mfsim.nam"
        lst_file = self.workspace / "mfsim.lst"

        # parallel test if configured
        if self.parallel:
            print(
                f"MODFLOW 6 parallel test {self.name} on",
                self.ncpus,
                "processes",
            )
            try:
                success, _ = self._run_parallel(exe)
            except Exception as exc:
                print(
                    SFMT.format("MODFLOW 6 parallel test", self.name, "failed")
                )
                print(exc)
                success = False
        else:
            # otherwise serial run
            try:
                print(SFMT.format("MODFLOW 6 test", self.name))
                success, _ = flopy.run_model(
                    exe, nam_file, model_ws=self.workspace
                )
            except Exception as exc:
                print(
                    SFMT.format("MODFLOW 6 serial test", self.name, "failed")
                )
                print(exc)
                success = False

        # set failure based on success and require_failure setting
        if self.xfail is None:
            msg = "MODFLOW 6 model did not terminate normally"
        else:
            if self.xfail:
                msg = "MODFLOW 6 model should have failed"
                success = not success
            else:
                msg = "MODFLOW 6 model should not have failed"

        if not success and lst_file.is_file():
            warn(f"{msg}\n" + self._get_mfsim_listing(lst_file))

        return success

    def run_comparison_model(self, comparison="compare"):
        """
        Run comparison model(s).

        comparison : str
            The comparison type
        """

        if comparison is None:
            raise ValueError(f"Comparison must be provided")

        # if default comparison (reference files), no way to tell which program to use, skip model run
        if comparison.lower() == "compare":
            warn(
                f"Comparison files provided separately, skipping model run for {self.name}",
            )
            return

        cmp_workspace = self.workspace / comparison
        cmp_key = comparison.lower().replace(".cmp", "")
        cmp_exe = str(self.targets[cmp_key].absolute())
        cmp_listfile = cmp_workspace / "mfsim.lst"
        self.cmp_namefile = (
            None
            if "mf6" in cmp_key
            or "libmf6" in cmp_key
            or "mf6_regression" in cmp_key
            else os.path.basename(get_namefiles(cmp_workspace)[0])
        )
        print(SFMT.format("comparison executable", cmp_exe))
        print(SFMT.format("comparison run", self.name + "/" + cmp_key))

        # run the model via API or per usual
        try:
            success, _ = (
                self.api_func(cmp_exe, cmp_workspace)
                if self.api_func
                else flopy.run_model(
                    cmp_exe,
                    self.cmp_namefile,
                    cmp_workspace,
                )
            )

            if not success and cmp_listfile.is_file():
                # print end of mfsim.lst
                if "mf6" in cmp_key:
                    warn(
                        "Comparison model run failed:\n"
                        + self._get_mfsim_listing(cmp_listfile)
                    )
        except:
            success = False
            warn(
                SFMT.format(
                    "Unhandled error in comparison model run ",
                    self.name + "/" + cmp_key,
                )
            )
            import traceback

            traceback.print_exc()

        return success

    def compare_output(self, comparison="compare"):
        """
        Compare the main model's output with a reference or regression model's output.

        comparison : str
            The comparison type
        """

        if comparison is None:
            raise ValueError(f"No comparison action")

        print(SFMT.format("Comparison test", self.name))

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
        cmp_path = self.workspace / comparison
        if "mf6" in comparison:
            _, self.coutp = get_mf6_files(cmp_path / "mfsim.nam")
        if "mf6_regression" in comparison:
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
                comparison=comparison, cpth=cmp_path, extensions=hds_ext
            ), "head comparison failed"

    def run(self):
        """
        Run the test case end-to-end.

        """

        # build model(s) and write input files
        if self.build:
            write_models(self.build(self))

        # run main model(s) and get expected output files
        assert self.run_main_model(), "main model(s) failed"
        _, self.outp = get_mf6_files(self.workspace / "mfsim.nam")

        # setup and run comparison model(s), if enabled
        if self.run_comparison:
            # adjust htol if it is smaller than IMS outer_dvclose
            dvclose = get_dvclose(self.workspace)
            if dvclose is not None:
                dvclose *= 5.0
                if self.htol < dvclose:
                    self.htol = dvclose

            # get rclose to use with budget comparisons
            rclose = get_rclose(self.workspace)
            if rclose is None:
                rclose = 0.5
            else:
                rclose *= 5.0
            self.rclose = rclose

            # copy mf6 regression files if needed
            if self.comparison == "mf6_regression":
                cmp_path = self.workspace / self.comparison
                if os.path.isdir(cmp_path):
                    shutil.rmtree(cmp_path)
                shutil.copytree(self.workspace, cmp_path)
            # detect comparison type if enabled
            else:
                self.comparison = get_mf6_comparison(self.workspace)

            # run comparison model if enabled
            if self.comparison:
                assert self.run_comparison_model(
                    self.comparison
                ), "comparison model(s) failed"
                if "mf6" in self.comparison:
                    _, self.coutp = get_mf6_files(self.workspace / "mfsim.nam")

        # compare model results, if enabled
        if self.make_comparison:
            if self.comparison:
                self.compare_output(self.comparison)
            else:
                warn(f"Comparison enabled but no action specified")

        # check results, if enabled
        if self.check:
            self.check(self)
