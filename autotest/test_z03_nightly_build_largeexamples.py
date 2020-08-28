import os
import sys
import nose

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

# find path to modflow6-largetestmodels directory
home = os.path.expanduser('~')
fdir = 'modflow6-largetestmodels'
exdir = None
for root, dirs, files in os.walk(home):
    for d in dirs:
        if d == fdir or d == fdir + '.git':
            exdir = os.path.abspath(os.path.join(root, d))
            break
    if exdir is not None:
        break


def get_mf6_models():
    """
        Get a list of test models
    """
    # determine if running on travis
    is_travis = 'TRAVIS' in os.environ
    is_github_action = 'CI' in os.environ

    # tuple of example files to exclude
    exclude = (None,)

    # update exclude
    if is_travis or is_github_action:
        exclude_CI = (None,)
        exclude = exclude + exclude_CI
    exclude = list(exclude)

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
                select_packages = [item.upper() for item in select_packages]
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
                ftype = pymake.get_mf6_ftypes(namefile, select_packages)
                if ftype not in ftypes:
                    ftypes += ftype
            if len(ftypes) > 0:
                ftypes = [item.upper() for item in ftypes]
                for pak in select_packages:
                    if pak in ftypes:
                        found_dirs.append(d)
                        break
        dirs = found_dirs
        if len(dirs) < 1:
            msg = 'Selected packages not available ['
            for pak in select_packages:
                msg += ' {}'.format(pak)
            msg += ']'
            print(msg)

    return dirs


def run_mf6(sim):
    """
    Run the MODFLOW 6 simulation and compare to existing head file or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    print(os.getcwd())
    src = os.path.join(exdir, sim.name)
    dst = os.path.join('temp', sim.name)
    sim.setup(src, dst)
    sim.run()
    sim.compare()
    sim.teardown()


def test_mf6model():
    # determine if largetest directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf6_models()

    # run the test models
    for dir in dirs:
        yield run_mf6, Simulation(dir)

    return


def dir_avail():
    avail = False
    if exdir is not None:
        avail = os.path.isdir(exdir)
    if not avail:
        print('"{}" does not exist'.format(exdir))
        print('no need to run {}'.format(os.path.basename(__file__)))
    if 'TRAVIS' in os.environ or 'CI' in os.environ:
        avail = False
    return avail


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = 'Running {} test'.format(tnam)
    print(msg)

    # determine if largetest directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf6_models()

    # run the test models
    for dir in dirs:
        sim = Simulation(dir)
        run_mf6(sim)

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
