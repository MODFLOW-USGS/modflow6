import os
import sys
import shutil
import time
import numpy as np

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

import targets

sfmt = "{:25s} - {}"
extdict = {
    "hds": "head",
    "hed": "head",
    "bhd": "head",
    "ucn": "concentration",
    "cbc": "cell-by-cell",
}


class Simulation(object):
    def __init__(
        self,
        name,
        exfunc=None,
        exe_dict=None,
        htol=None,
        pdtol=None,
        rclose=None,
        idxsim=None,
        cmp_verbose=True,
        require_failure=None,
        bmifunc=None,
        mf6_regression=False,
    ):
        delFiles = True
        for idx, arg in enumerate(sys.argv):
            if arg.lower() == "--keep":
                delFiles = False
            elif arg[2:].lower() in list(targets.target_dict.keys()):
                key = arg[2:].lower()
                exe0 = targets.target_dict[key]
                exe = os.path.join(os.path.dirname(exe0), sys.argv[idx + 1])
                msg = (
                    "replacing {} executable ".format(key)
                    + '"{}" with '.format(targets.target_dict[key])
                    + '"{}".'.format(exe)
                )
                print(msg)
                targets.target_dict[key] = exe

        if exe_dict is not None:
            if not isinstance(exe_dict, dict):
                msg = "exe_dict must be a dictionary"
                assert False, msg
            keys = list(targets.target_dict.keys())
            for key, value in exe_dict.items():
                if key in keys:
                    exe0 = targets.target_dict[key]
                    exe = os.path.join(os.path.dirname(exe0), value)
                    msg = (
                        "replacing {} executable ".format(key)
                        + '"{}" with '.format(targets.target_dict[key])
                        + '"{}".'.format(exe)
                    )
                    print(msg)
                    targets.target_dict[key] = exe

        msg = sfmt.format("Initializing test", name)
        print(msg)
        self.name = name
        self.exfunc = exfunc
        self.simpath = None
        self.inpt = None
        self.outp = None
        self.coutp = None
        self.bmifunc = bmifunc
        self.mf6_regression = mf6_regression
        self.action = None

        # set htol for comparisons
        if htol is None:
            htol = 0.001
        else:
            msg = sfmt.format("User specified comparison htol", htol)
            print(msg)

        self.htol = htol

        # set pdtol for comparisons
        if pdtol is None:
            pdtol = 0.001
        else:
            msg = sfmt.format(
                "User specified percent difference comparison pdtol", pdtol
            )
            print(msg)

        self.pdtol = pdtol

        # set rclose for comparisons
        if rclose is None:
            rclose = 0.001
        else:
            msg = sfmt.format(
                "User specified percent difference comparison rclose", rclose
            )
            print(msg)

        self.rclose = rclose

        # set index for multi-simulation comparisons
        self.idxsim = idxsim

        # set compare verbosity
        self.cmp_verbose = cmp_verbose

        # set allow failure
        self.require_failure = require_failure

        self.delFiles = delFiles
        self.success = False
        return

    def __repr__(self):
        return self.name

    def set_model(self, pth, testModel=True):
        """
        Set paths to MODFLOW 6 model and associated comparison test
        """
        # make sure this is a valid path
        if not os.path.isdir(pth):
            assert False, "{} is not a valid directory".format(pth)

        self.simpath = pth

        # get MODFLOW 6 output file names
        fpth = os.path.join(pth, "mfsim.nam")
        mf6inp, mf6outp = pymake.get_mf6_files(fpth)
        self.outp = mf6outp

        # determine comparison model
        self.setup_comparison(pth, pth, testModel=testModel)
        # if self.mf6_regression:
        #     self.action = "mf6-regression"
        # else:
        #     self.action = pymake.get_mf6_comparison(pth)
        if self.action is not None:
            if "mf6" in self.action or "mf6-regression" in self.action:
                cinp, self.coutp = pymake.get_mf6_files(fpth)

    def setup(self, src, dst):
        msg = sfmt.format("Setup test", self.name)
        print(msg)
        self.originpath = src
        self.simpath = dst
        # write message
        print(
            "running pymake.setup_mf6 from "
            + "{}".format(os.path.abspath(os.getcwd()))
        )
        try:
            self.inpt, self.outp = pymake.setup_mf6(src=src, dst=dst)
            print("waiting...")
            time.sleep(0.5)
            success = True
        except:
            success = False
            print("source:      {}".format(src))
            print("destination: {}".format(dst))
        assert success, "did not run pymake.setup_mf6"

        if success:
            self.setup_comparison(src, dst)

        return

    def setup_comparison(self, src, dst, testModel=True):

        # adjust htol if it is smaller than IMS outer_dvclose
        dvclose = self._get_dvclose(dst)
        if dvclose is not None:
            dvclose *= 5.0
            if self.htol < dvclose:
                self.htol = dvclose

        # get rclose to use with budget comparisons
        rclose = self._get_rclose(dst)
        if rclose is None:
            rclose = 0.5
        else:
            rclose *= 5.0
        self.rclose = rclose

        # Copy comparison simulations if available
        if self.mf6_regression:
            action = "mf6-regression"
            pth = os.path.join(dst, action)
            if os.path.isdir(pth):
                shutil.rmtree(pth)
            shutil.copytree(dst, pth)
        elif testModel:
            action = pymake.setup_mf6_comparison(
                src, dst, remove_existing=self.delFiles
            )
        else:
            action = pymake.get_mf6_comparison(dst)

        self.action = action

        return

    def run(self):
        """
        Run the model and assert if the model terminated successfully
        """
        msg = sfmt.format("Run test", self.name)
        print(msg)

        # Set nam as namefile name without path
        nam = None

        # run mf6 models
        target, ext = os.path.splitext(targets.program)
        exe = os.path.abspath(targets.target_dict[target])
        msg = sfmt.format("using executable", exe)
        print(msg)
        try:
            success, buff = flopy.run_model(
                exe, nam, model_ws=self.simpath, silent=False, report=True
            )
            msg = sfmt.format("MODFLOW 6 run", self.name)
            if success:
                print(msg)
            else:
                print(msg)
        except:
            msg = sfmt.format("MODFLOW 6 run", self.name)
            print(msg)
            success = False

        if self.require_failure is None:
            assert success, "MODFLOW 6 model did not terminate normally"
        else:
            if self.require_failure:
                assert success is False, "MODFLOW 6 model should have failed"
            else:
                assert (
                    success is True
                ), "MODFLOW 6 model should not have failed"

        self.nam_cmp = None
        if success:
            if self.action is not None:
                if self.action.lower() == "compare":
                    msg = sfmt.format("Comparison files", self.name)
                    print(msg)
                else:
                    cpth = os.path.join(self.simpath, self.action)
                    key = self.action.lower().replace(".cmp", "")
                    exe = os.path.abspath(targets.target_dict[key])
                    if (
                        "mf6" in key
                        or "libmf6" in key
                        or "mf6-regression" in key
                    ):
                        nam = None
                    else:
                        npth = pymake.get_namefiles(cpth)[0]
                        nam = os.path.basename(npth)
                    self.nam_cmp = nam
                    try:
                        if self.bmifunc is None:
                            success_cmp, buff = flopy.run_model(
                                exe,
                                nam,
                                model_ws=cpth,
                                silent=False,
                                report=True,
                            )
                        else:
                            success_cmp, buff = self.bmifunc(
                                exe, self.idxsim, model_ws=cpth
                            )
                        msg = sfmt.format(
                            "Comparison run", self.name + "/" + key
                        )
                        if success:
                            print(msg)
                        else:
                            print(msg)
                    except:
                        success_cmp = False
                        msg = sfmt.format(
                            "Comparison run", self.name + "/" + key
                        )
                        print(msg)

                    assert success_cmp, "Unsuccessful comparison"

        return

    def compare(self):
        """
        Compare the model results

        """
        self.success = True
        msgall = ""
        msg = sfmt.format("Comparison test", self.name)
        print(msg)

        success_tst = False
        if self.action is not None:
            cpth = os.path.join(self.simpath, self.action)
            files_cmp = None
            if self.action.lower() == "compare":
                files_cmp = []
                files = os.listdir(cpth)
                for file in files:
                    files_cmp.append(file)
            elif "mf6" in self.action:
                fpth = os.path.join(cpth, "mfsim.nam")
                cinp, self.coutp = pymake.get_mf6_files(fpth)

            head_extensions = (
                "hds",
                "hed",
                "bhd",
                "ahd",
                "bin",
            )
            if "mf6-regression" in self.action:
                success, msgall = self._compare_heads(
                    msgall,
                    extensions=head_extensions,
                )
                if not success:
                    self.success = False
            # non-regression runs - for new features
            else:
                files1 = []
                files2 = []
                exfiles = []
                ipos = 0
                for file1 in self.outp:
                    ext = os.path.splitext(file1)[1][1:]

                    if ext.lower() in head_extensions:

                        # simulation file
                        pth = os.path.join(self.simpath, file1)
                        files1.append(pth)

                        # look for an exclusion file
                        pth = os.path.join(self.simpath, file1 + ".ex")
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
                                txt = sfmt.format(
                                    "Comparison file {}".format(ipos + 1),
                                    os.path.basename(pth),
                                )
                                print(txt)
                        else:
                            if self.coutp is not None:
                                for file2 in self.coutp:
                                    ext = os.path.splitext(file2)[1][1:]

                                    if ext.lower() in head_extensions:
                                        # simulation file
                                        pth = os.path.join(cpth, file2)
                                        files2.append(pth)

                            else:
                                files2.append(None)

                if self.nam_cmp is None:
                    pth = None
                else:
                    pth = os.path.join(cpth, self.nam_cmp)

                for ipos in range(len(files1)):
                    file1 = files1[ipos]
                    ext = os.path.splitext(file1)[1][1:].lower()
                    outfile = os.path.splitext(os.path.basename(file1))[0]
                    outfile = os.path.join(
                        self.simpath, outfile + "." + ext + ".cmp.out"
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
                                txt = sfmt.format(
                                    "Exclusion file {}".format(ipos + 1),
                                    os.path.basename(exfile),
                                )
                                print(txt)

                    # make comparison
                    success_tst = pymake.compare_heads(
                        None,
                        pth,
                        precision="double",
                        text=extdict[ext],
                        outfile=outfile,
                        files1=file1,
                        files2=file2,
                        htol=self.htol,
                        difftol=True,
                        # Change to true to have list of all nodes exceeding htol
                        verbose=self.cmp_verbose,
                        exfile=exfile,
                    )
                    msg = sfmt.format(
                        "{} comparison {}".format(extdict[ext], ipos + 1),
                        self.name,
                    )
                    print(msg)

                    if not success_tst:
                        self.success = False
                        msgall += msg + " ... FAILED\n"

            # compare concentrations
            if "mf6-regression" in self.action:
                success, msgall = self._compare_concentrations(msgall)
                if not success:
                    self.success = False

            # compare cbc files
            if "mf6-regression" in self.action:
                cbc_extensions = (
                    "cbc",
                    "bud",
                )
                success, msgall = self._compare_budgets(
                    msgall, extensions=cbc_extensions
                )
                if not success:
                    self.success = False

        assert self.success, msgall
        return

    def teardown(self):
        """
        Remove the example folder

        """
        if self.success:
            if self.delFiles:
                msg = sfmt.format("Teardown test", self.name)
                print(msg)

                # wait to delete on windows
                if sys.platform.lower() == "win32":
                    time.sleep(3)

                try:
                    shutil.rmtree(self.simpath)
                    success = True
                except:
                    print("Could not remove test " + self.name)
                    success = False
                assert success
            else:
                print("Retaining test files")
        return

    def _get_dvclose(self, dir_pth):
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

    def _get_rclose(self, dir_pth):
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

    def _regression_files(self, extensions):
        if isinstance(extensions, str):
            extensions = [extensions]
        files = os.listdir(self.simpath)
        files0 = []
        files1 = []
        for file_name in files:
            fpth0 = os.path.join(self.simpath, file_name)
            if os.path.isfile(fpth0):
                for extension in extensions:
                    if file_name.lower().endswith(extension):
                        files0.append(fpth0)
                        fpth1 = os.path.join(
                            self.simpath, "mf6-regression", file_name
                        )
                        files1.append(fpth1)
                        break
        return files0, files1

    def _compare_heads(self, msgall, extensions="hds"):
        if isinstance(extensions, str):
            extensions = [extensions]
        success = True
        files0, files1 = self._regression_files(extensions)
        extension = "hds"
        ipos = 0
        for idx, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.simpath, outfile + ".{}.cmp.out".format(extension)
            )
            success_tst = pymake.compare_heads(
                None,
                None,
                precision="double",
                htol=self.htol,
                text=extdict[extension],
                outfile=outfile,
                files1=fpth0,
                files2=fpth1,
                verbose=self.cmp_verbose,
            )
            msg = sfmt.format(
                "{} comparison {}".format(extdict[extension], ipos + 1),
                "{} ({})".format(self.name, os.path.basename(fpth0)),
            )
            ipos += 1
            print(msg)

            if not success_tst:
                success = False
                msgall += msg + " ... FAILED\n"

        return success, msgall

    def _compare_concentrations(self, msgall, extensions="ucn"):
        if isinstance(extensions, str):
            extensions = [extensions]
        success = True
        files0, files1 = self._regression_files(extensions)
        extension = "ucn"
        ipos = 0
        for idx, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.simpath, outfile + ".{}.cmp.out".format(extension)
            )
            success_tst = pymake.compare_heads(
                None,
                None,
                precision="double",
                htol=self.htol,
                text=extdict[extension],
                outfile=outfile,
                files1=fpth0,
                files2=fpth1,
                verbose=self.cmp_verbose,
            )
            msg = sfmt.format(
                "{} comparison {}".format(extdict[extension], ipos + 1),
                "{} ({})".format(self.name, os.path.basename(fpth0)),
            )
            ipos += 1
            print(msg)

            if not success_tst:
                success = False
                msgall += msg + " ... FAILED\n"

        return success, msgall

    def _compare_budgets(self, msgall, extensions="cbc"):
        if isinstance(extensions, str):
            extensions = [extensions]
        success = True
        files0, files1 = self._regression_files(extensions)
        extension = "cbc"
        ipos = 0
        for idx, (fpth0, fpth1) in enumerate(zip(files0, files1)):
            if os.stat(fpth0).st_size * os.stat(fpth0).st_size == 0:
                continue
            outfile = os.path.splitext(os.path.basename(fpth0))[0]
            outfile = os.path.join(
                self.simpath, outfile + ".{}.cmp.out".format(extension)
            )
            fcmp = open(outfile, "w")

            # open the files
            cbc0 = flopy.utils.CellBudgetFile(
                fpth0, precision="double", verbose=self.cmp_verbose
            )
            cbc1 = flopy.utils.CellBudgetFile(
                fpth1, precision="double", verbose=self.cmp_verbose
            )

            # build list of cbc data to retrieve
            avail = cbc0.get_unique_record_names()

            # initialize list for storing totals for each budget term terms
            cbc_keys = []
            for t in avail:
                if isinstance(t, bytes):
                    t = t.decode()
                t = t.strip()
                # remove these checks once v6.2.2 is released
                if t.startswith("FLOW-JA-FACE") or t.startswith("UZF-GWRCH"):
                    continue
                cbc_keys.append(t)

            # get list of times and kstpkper
            kk = cbc0.get_kstpkper()
            times = cbc0.get_times()

            # process data
            success_tst = True
            for key in cbc_keys:
                for idx, (k, t) in enumerate(zip(kk, times)):
                    v0 = cbc0.get_data(kstpkper=k, text=key)[0]
                    v1 = cbc1.get_data(kstpkper=k, text=key)[0]
                    if v0.dtype.names is not None:
                        v0 = v0["q"]
                        v1 = v1["q"]
                    # skip empty vectors
                    if v0.size < 1:
                        continue
                    vmin = self.rclose
                    if vmin < 1e-6:
                        vmin = 1e-6
                    idx = (abs(v0) > vmin) & (abs(v1) > vmin)
                    percent_diff = np.zeros(v0.shape, dtype=v0.dtype)
                    percent_diff[idx] = (
                        100.0 * abs(v0[idx] - v1[idx]) / abs(v0[idx])
                    )
                    percent_diffmax = percent_diff.max()
                    indices = np.where(percent_diff == percent_diffmax)[0]
                    if percent_diffmax > self.pdtol:
                        success_tst = False
                        msg = (
                            "{} - ".format(os.path.basename(fpth0))
                            + "{:16s} ".format(key)
                            + "difference ({:10.4g}) ".format(percent_diffmax)
                            + "> {:10.4g} ".format(self.pdtol)
                            + "at {} nodes ".format(indices.size)
                            + " [first location ({})] ".format(indices[0] + 1)
                            + "at time {} maximum percent ".format(t)
                        )
                        fcmp.write("{}\n".format(msg))
                        if self.cmp_verbose:
                            print(msg)

            msg = sfmt.format(
                "{} comparison {}".format(extdict[extension], ipos + 1),
                "{} ({})".format(self.name, os.path.basename(fpth0)),
            )
            ipos += 1
            print(msg)

            fcmp.close()

            if not success_tst:
                success = False
                msgall += msg + " ... FAILED\n"

        return success, msgall


def bmi_return(success, model_ws):
    """
    parse libmf6.so and libmf6.dll stdout file
    """
    fpth = os.path.join(model_ws, "mfsim.stdout")
    return success, open(fpth).readlines()
