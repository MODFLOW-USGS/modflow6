import os
import sys
import pathlib

import time
import shutil

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

from simulation import Simulation

from targets import target_dict as target_dict


def get_example_directory(base, fdir, subdir='mf6'):
    exdir = None
    for root, dirs, files in os.walk(base):
        for d in dirs:
            if d.startswith(fdir):
                exdir = os.path.abspath(os.path.join(root, d, subdir))
                break
        if exdir is not None:
            break
    return exdir


# find path to modflow6-testmodels or modflow6-testmodels.git directory
home = os.path.expanduser('~')
print('$HOME={}'.format(home))

fdir = 'modflow6-testmodels'
exdir = get_example_directory(home, fdir, subdir='mf5to6')
if exdir is None:
    p = pathlib.Path(os.getcwd())
    home = os.path.abspath(pathlib.Path(*p.parts[:2]))
    print('$HOME={}'.format(home))
    exdir = get_example_directory(home, fdir, subdir='mf5to6')

if exdir is not None:
    assert os.path.isdir(exdir)

sfmt = '{:25s} - {}'


def get_mf5to6_models():
    """
        Get a list of test models
    """
    # list of example files to exclude
    exclude = [
        'test1ss_ic1',
        'test9.5-3layer',
        'testmm2',
        'testmm3',
        'testmmSimple',
        'testps3a',
        'testTwri',
        'testTwrip',
        'test028_sfr_simple',
    ]
    # os_name = sys.platform.lower()
    # if os_name in ("win32", "linux", "darwin"):
    #     exclude.append("testlgrsfr")
    # elif os_name in ("linux", "darwin"):
    #     exclude.append("test059_mvlake_laksfr_tr")

    # write a summary of the files to exclude
    print('list of tests to exclude:')
    for idx, ex in enumerate(exclude):
        print('    {}: {}'.format(idx + 1, ex))

    # build list of directories with valid example files
    if exdir is not None:
        dirs = [d for d in os.listdir(exdir)
                if 'test' in d and d not in exclude]
        # sort in numerical order for case sensitive os
        dirs = sorted(dirs, key=lambda v: (v.upper(), v[0].islower()))
    else:
        dirs = []

    # determine if only a selection of models should be run
    select_dirs = None
    select_packages = None
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--sim':
            if len(sys.argv) > idx + 1:
                select_dirs = sys.argv[idx + 1:]
                break
        elif arg.lower() == '--pak':
            if len(sys.argv) > idx + 1:
                select_packages = sys.argv[idx + 1:]
                select_packages = [item.upper() for item in
                                   select_packages]
                break

    # determine if the selection of model is in the test models to evaluate
    if select_dirs is not None:
        found_dirs = []
        for d in select_dirs:
            if d in dirs:
                found_dirs.append(d)
        dirs = found_dirs
        if len(dirs) < 1:
            msg = 'Selected models not available in test'
            print(msg)

    # determine if the specified package(s) is in the test models to evaluate
    if select_packages is not None:
        found_dirs = []
        for d in dirs:
            pth = os.path.join(exdir, d)
            namefiles = pymake.get_namefiles(pth)
            ftypes = []
            for namefile in namefiles:
                for pak in select_packages:
                    ftype = pymake.get_entries_from_namefile(namefile,
                                                             ftype=pak)
                    for t in ftype:
                        if t[1] is not None:
                            if t[1] not in ftypes:
                                ftypes.append(t[1].upper())
            if len(ftypes) > 0:
                ftypes = [item.upper() for item in ftypes]
                for pak in select_packages:
                    if pak in ftypes:
                        found_dirs.append(d)
                        break
        dirs = found_dirs
        if len(dirs) < 1:
            msg = 'Selected packages not available ['
            for idx, pak in enumerate(select_packages):
                msg += '{}'.format(pak)
                if idx + 1 < len(select_packages):
                    msg += ', '
            msg += ']'
            print(msg)

    return dirs


def run_mf5to6(sim):
    """
    Run the MODFLOW 6 simulation and compare to existing head file or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    src = os.path.join(exdir, sim.name)
    dst = os.path.join('temp', 'working')

    # set default version
    version = 'mf2005'
    lgrpth = None
    compare = False
    cpth = None

    # determine if compare directory exists in directory or if mflgr control
    # file is in directory
    listdir = os.listdir(src)
    for value in listdir:
        fpth = os.path.join(src, value)
        if os.path.isfile(fpth):
            ext = os.path.splitext(fpth)[1]
            if '.lgr' in ext.lower():
                version = 'mflgr'
                lgrpth = fpth
        elif os.path.isdir(fpth):
            if 'compare' in value.lower() or 'cmp' in value.lower():
                compare = True
                cpth = value

    msg = 'Copying {} files to working directory'.format(version)
    # copy lgr files to working directory
    if lgrpth is not None:
        print(msg)
        npth = lgrpth
        pymake.setup(lgrpth, dst)
    # copy modflow 2005, NWT, or USG files to working directory
    else:
        print(msg)
        npths = pymake.get_namefiles(src)
        if len(npths) < 1:
            msg = 'No name files in {}'.format(src)
            print(msg)
            assert False
        npth = npths[0]
        pymake.setup(npth, dst)

    # read ftype from name file to set modflow version
    if version != 'mflgr':
        lines = [line.rstrip('\n') for line in open(npth)]
        for line in lines:
            if len(line) < 1:
                continue
            t = line.split()
            ftype = t[0].upper()
            if ftype == 'NWT' or ftype == 'UPW':
                version = 'mfnwt'
                break
            elif ftype == 'SMS' or ftype == 'DISU':
                version = 'mfusg'
                break

    # run converter
    exe = os.path.abspath(target_dict['mf5to6'])
    msg = sfmt.format('using executable', exe)
    print(msg)
    nmsg = 'Program terminated normally'
    try:
        nam = os.path.basename(npth)
        success, buff = flopy.run_model(exe, nam, model_ws=dst,
                                        silent=False, report=True,
                                        normal_msg=nmsg,
                                        cargs='mf6')
        msg = sfmt.format('MODFLOW 5 to 6 run', nam)
        if success:
            print(msg)
        else:
            print('ERROR: ' + msg)
    except:
        msg = sfmt.format('MODFLOW 5 to 6 run', nam)
        print('ERROR: ' + msg)
        success = False

    assert success, msg

    # path to copy

    # copy files in the compare directory
    if compare:
        dst2 = os.path.join(dst, cpth)
        tpth = os.path.join(exdir, sim.name, cpth)
        shutil.copytree(tpth, dst2)
    # copy original modflow files to the appopriate directory
    # (mf2005, mfnwt, or mfusg) in temp/working
    else:
        dst2 = os.path.join(dst, version)
        pymake.setup(npth, dst2)

    # standard setup
    src = dst
    dst = os.path.join('temp', sim.name)
    sim.setup(src, dst)

    # clean up temp/working directory (src)
    if os.path.exists(src):
        msg = 'Removing {} directory'.format(src)
        print(msg)
        shutil.rmtree(src)
        time.sleep(0.5)

    # standard comparison run
    sim.run()
    sim.compare()
    sim.teardown()


def test_model():
    # determine if test directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf5to6_models()

    # run the test models
    for dir in dirs:
        yield run_mf5to6, Simulation(dir)

    return


def dir_avail():
    avail = False
    if exdir is not None:
        avail = os.path.isdir(exdir)
    if not avail:
        print('"{}" does not exist'.format(exdir))
        print('no need to run {}'.format(os.path.basename(__file__)))
    return avail


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = 'Running {} test'.format(tnam)
    print(msg)

    # get name of current file
    module_name = sys.modules[__name__].__file__

    # determine if test directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf5to6_models()

    # run the test models
    for dir in dirs:
        sim = Simulation(dir)
        run_mf5to6(sim)

    return


if __name__ == "__main__":

    print('standalone run of {}'.format(os.path.basename(__file__)))

    delFiles = True
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--keep':
            if len(sys.argv) > idx + 1:
                delFiles = False
                break

    # run main routine
    main()
