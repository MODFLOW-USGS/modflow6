import os
import sys
import shutil
import time

try:
    import pymake
except:
    msg = 'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    raise Exception(msg)

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

import targets

sfmt = '{:25s} - {}'


class Simulation(object):
    def __init__(self, name, exfunc=None, exe_dict=None, htol=None,
                 idxsim=None, cmp_verbose=True, require_failure=None,
                 bmifunc=None):
        delFiles = True
        for idx, arg in enumerate(sys.argv):
            if arg.lower() == '--keep':
                delFiles = False
            elif arg[2:].lower() in list(targets.target_dict.keys()):
                key = arg[2:].lower()
                exe0 = targets.target_dict[key]
                exe = os.path.join(os.path.dirname(exe0), sys.argv[idx + 1])
                msg = 'replacing {} executable '.format(key) + \
                      '"{}" with '.format(targets.target_dict[key]) + \
                      '"{}".'.format(exe)
                print(msg)
                targets.target_dict[key] = exe

        if exe_dict is not None:
            if not isinstance(exe_dict, dict):
                msg = 'exe_dict must be a dictionary'
                assert False, msg
            keys = list(targets.target_dict.keys())
            for key, value in exe_dict.items():
                if key in keys:
                    exe0 = targets.target_dict[key]
                    exe = os.path.join(os.path.dirname(exe0), value)
                    msg = 'replacing {} executable '.format(key) + \
                          '"{}" with '.format(targets.target_dict[key]) + \
                          '"{}".'.format(exe)
                    print(msg)
                    targets.target_dict[key] = exe

        msg = sfmt.format('Initializing test', name)
        print(msg)
        self.name = name
        self.exfunc = exfunc
        self.simpath = None
        self.inpt = None
        self.outp = None
        self.coutp = None
        self.bmifunc = bmifunc

        # set htol for comparisons
        if htol is None:
            htol = 0.001
        else:
            msg = sfmt.format('User specified comparison htol', htol)
            print(msg)

        self.htol = htol

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

    def set_model(self, pth):
        """
        Set paths to MODFLOW 6 model and associated comparison test
        """
        # make sure this is a valid path
        if not os.path.isdir(pth):
            assert False, '{} is not a valid directory'.format(pth)

        self.simpath = pth

        # get MODFLOW 6 output file names
        fpth = os.path.join(pth, 'mfsim.nam')
        mf6inp, mf6outp = pymake.get_mf6_files(fpth)
        self.outp = mf6outp

        # determine comparison model
        self.action = pymake.get_mf6_comparison(pth)
        if self.action is not None:
            if 'mf6' in self.action:
                cinp, self.coutp = pymake.get_mf6_files(fpth)

    def setup(self, src, dst):
        msg = sfmt.format('Setup test', self.name)
        print(msg)
        self.originpath = src
        self.simpath = dst
        # write message
        print('running pymake.setup_mf6 from ' +
              '{}'.format(os.path.abspath(os.getcwd())))
        try:
            self.inpt, self.outp = pymake.setup_mf6(src=src, dst=dst)
            print('waiting...')
            time.sleep(0.5)
            success = True
        except:
            success = False
            print('source:      {}'.format(src))
            print('destination: {}'.format(dst))
        assert success, 'did not run pymake.setup_mf6'

        # Copy comparison simulations if available
        if success:
            action = pymake.setup_mf6_comparison(src, dst,
                                                 remove_existing=self.delFiles)

            self.action = action
        return

    def run(self):
        """
        Run the model and assert if the model terminated successfully
        """
        msg = sfmt.format('Run test', self.name)
        print(msg)

        # Set nam as namefile name without path
        nam = None

        # run mf6 models
        target, ext = os.path.splitext(targets.program)
        exe = os.path.abspath(targets.target_dict[target])
        msg = sfmt.format('using executable', exe)
        print(msg)
        try:
            success, buff = flopy.run_model(exe, nam, model_ws=self.simpath,
                                            silent=False, report=True)
            msg = sfmt.format('MODFLOW 6 run', self.name)
            if success:
                print(msg)
            else:
                print(msg)
        except:
            msg = sfmt.format('MODFLOW 6 run', self.name)
            print(msg)
            success = False

        if self.require_failure is None:
            assert success, "MODFLOW 6 model did not terminate normally"
        else:
            if self.require_failure:
                assert success is False, "MODFLOW 6 model should have failed"
            else:
                assert success is True, "MODFLOW 6 model should not have failed"

        self.nam_cmp = None
        if success:
            if self.action is not None:
                if self.action.lower() == 'compare':
                    msg = sfmt.format('Comparison files', self.name)
                    print(msg)
                else:
                    cpth = os.path.join(self.simpath, self.action)
                    key = self.action.lower().replace('.cmp', '')
                    exe = os.path.abspath(targets.target_dict[key])
                    if 'mf6' in key or 'libmf6' in key:
                        nam = None
                    else:
                        npth = pymake.get_namefiles(cpth)[0]
                        nam = os.path.basename(npth)
                    self.nam_cmp = nam
                    try:
                        if self.bmifunc is None:
                            success_cmp, buff = flopy.run_model(exe, nam,
                                                                model_ws=cpth,
                                                                silent=False,
                                                                report=True)
                        else:
                            success_cmp, buff = self.bmifunc(exe,
                                                             self.idxsim,
                                                             model_ws=cpth)
                        msg = sfmt.format('Comparison run',
                                          self.name + '/' + key)
                        if success:
                            print(msg)
                        else:
                            print(msg)
                    except:
                        success_cmp = False
                        msg = sfmt.format('Comparison run',
                                          self.name + '/' + key)
                        print(msg)

                    assert success_cmp, "Unsuccessful comparison"

        return

    def compare(self):
        """
        Compare the model results

        """
        self.success = True
        msgall = ''
        msg = sfmt.format('Comparison test', self.name)
        print(msg)

        extdict = {'hds': 'head', 'hed': 'head', 'bhd': 'head',
                   'ucn': 'concentration'}

        success_tst = False
        if self.action is not None:
            cpth = os.path.join(self.simpath, self.action)
            files_cmp = None
            if self.action.lower() == 'compare':
                files_cmp = []
                files = os.listdir(cpth)
                for file in files:
                    files_cmp.append(file)
            elif 'mf6' in self.action:
                fpth = os.path.join(cpth, 'mfsim.nam')
                cinp, self.coutp = pymake.get_mf6_files(fpth)

            files1 = []
            files2 = []
            exfiles = []
            ipos = 0
            for file1 in self.outp:
                ext = os.path.splitext(file1)[1][1:]

                if ext.lower() in ['hds', 'hed', 'bhd', 'ahd']:

                    # simulation file
                    pth = os.path.join(self.simpath, file1)
                    files1.append(pth)

                    # look for an exclusion file
                    pth = os.path.join(self.simpath, file1 + '.ex')
                    if os.path.isfile(pth):
                        exfiles.append(pth)
                    else:
                        exfiles.append(None)

                    # Check to see if there is a corresponding compare file
                    if files_cmp is not None:

                        if file1 + '.cmp' in files_cmp:
                            # compare file
                            idx = files_cmp.index(file1 + '.cmp')
                            pth = os.path.join(cpth, files_cmp[idx])
                            files2.append(pth)
                            txt = sfmt.format(
                                'Comparison file {}'.format(ipos + 1),
                                os.path.basename(pth))
                            print(txt)
                    else:
                        if self.coutp is not None:
                            for file2 in self.coutp:
                                ext = os.path.splitext(file2)[1][1:]

                                if ext.lower() in ['hds', 'hed', 'bhd', 'ahd']:
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
                outfile = os.path.join(self.simpath, outfile + '.' + ext +
                                       '.cmp.out')
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
                                'Exclusion file {}'.format(ipos + 1),
                                os.path.basename(exfile))
                            print(txt)

                # make comparison
                success_tst = pymake.compare_heads(None, pth,
                                                   precision='double',
                                                   text=extdict[ext],
                                                   outfile=outfile,
                                                   files1=file1,
                                                   files2=file2,
                                                   htol=self.htol,
                                                   difftol=True,
                                                   # Change to true to have list of all nodes exceeding htol
                                                   verbose=self.cmp_verbose,
                                                   exfile=exfile)
                msg = sfmt.format('{} comparison {}'.format(extdict[ext],
                                                            ipos + 1),
                                  self.name)
                if success_tst:
                    print(msg)
                else:
                    print(msg)

                if not success_tst:
                    self.success = False
                    msgall += msg + '\n'

        assert self.success, msgall
        return

    def teardown(self):
        """
        Remove the example folder

        """
        if self.success:
            if self.delFiles:
                msg = sfmt.format('Teardown test', self.name)
                print(msg)

                # wait to delete on windows
                if sys.platform.lower() == "win32":
                    time.sleep(3)

                try:
                    shutil.rmtree(self.simpath)
                    success = True
                except:
                    print('Could not remove test ' + self.name)
                    success = False
                assert success
            else:
                print('Retaining test files')
        return


def bmi_return(success, model_ws):
    """
    parse libmf6.so and libmf6.dll stdout file
    """
    fpth = os.path.join(model_ws, 'mfsim.stdout')
    return success, open(fpth).readlines()
