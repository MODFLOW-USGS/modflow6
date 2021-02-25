import os
import sys
import subprocess
import pathlib

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
exdir = get_example_directory(home, fdir, subdir='mf6')
if exdir is None:
    p = pathlib.Path(os.getcwd())
    home = os.path.abspath(pathlib.Path(*p.parts[:2]))
    print('$HOME={}'.format(home))
    exdir = get_example_directory(home, fdir, subdir='mf6')

if exdir is not None:
    assert os.path.isdir(exdir)


def get_branch():
    try:
        # determine current buildstat branch
        b = subprocess.Popen(("git", "status"),
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT).communicate()[0]
        if isinstance(b, bytes):
            b = b.decode('utf-8')

        # determine current buildstat branch
        for line in b.splitlines():
            if 'On branch' in line:
                branch = line.replace('On branch ', '').rstrip()
    except:
        branch = None

    return branch


def get_mf6_models():
    """
        Get a list of test models
    """
    # determine if running on travis
    is_travis = 'TRAVIS' in os.environ
    is_github_action = 'CI' in os.environ

    # get current branch
    is_CI = False
    if is_travis:
        is_CI = True
        branch = os.environ['BRANCH']
    elif is_github_action:
        is_CI = True
        branch = os.path.basename(os.environ['GITHUB_REF'])
    else:
        branch = get_branch()
    print('On branch {}'.format(branch))

    # tuple of example files to exclude
    exclude = (None,)

    # update exclude
    if is_CI:
        exclude_CI = ('test022_MNW2_Fig28',
                      'test007_751x751_confined')
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
    else:
        dirs = []

    # exclude dev examples on master or release branches
    if 'master' in branch.lower() or 'release' in branch.lower():
        drmv = []
        for d in dirs:
            if '_dev' in d.lower():
                drmv.append(d)
        for d in drmv:
            dirs.remove(d)

    # sort in numerical order for case sensitive os
    if len(dirs) > 0:
        dirs = sorted(dirs, key=lambda v: (v.upper(), v[0].islower()))

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
        elif arg.lower() == '--match':
            if len(sys.argv) > idx + 1:
                like = sys.argv[idx + 1]
                dirs = [item for item in dirs if like in item]
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


def get_htol(dir):
    htol = None
    htol_dict = {
        "test059_mvlake_laksfr_tr": {
            "linux": 0.002,
            "darwin": 0.002,
        },
        "test045_lake4ss": {
            "linux": 0.007,
            "darwin": 0.007,
            "win32": 0.007,
        },
        "test045_lake4ss_nr_dev": {
            "linux": 0.002,
            "darwin": 0.002,
            "win32": 0.002,
        },

    }
    htol_keys = list(htol_dict.keys())
    if dir in htol_keys:
        dir_dict = htol_dict[dir]
        os_keys = list(dir_dict.keys())
        os_key = sys.platform.lower()
        if os_key in os_keys:
            htol = dir_dict[os_key]
    return htol


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
    # determine if test directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf6_models()

    # run the test models
    for dir in dirs:
        yield run_mf6, Simulation(dir, htol=get_htol(dir))

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

    # determine if test directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf6_models()

    # run the test models
    for dir in dirs:
        sim = Simulation(dir, htol=get_htol(dir))
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
