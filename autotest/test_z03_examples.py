import os
import sys

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

from simulation import Simulation
from common_regression import get_select_dirs, get_select_packages

# find path to modflow6-examples directory
home = os.path.expanduser("~")
fdir = "modflow6-examples"
exdir = None
for root, dirs, files in os.walk(home):
    for d in dirs:
        if d == fdir or d == fdir + ".git":
            exdir = os.path.abspath(os.path.join(root, d, "examples"))
            break
    if exdir is not None:
        break


def get_mf6_models():
    """
    Get a list of test models
    """
    # determine if running on travis
    is_travis = "TRAVIS" in os.environ
    is_github_action = "CI" in os.environ

    # tuple of example files to exclude
    exclude = ("ex-gwf-csub-p02c",)

    # update exclude
    if is_travis or is_github_action:
        exclude_CI = (None,)
        exclude = exclude + exclude_CI
    exclude = list(exclude)

    # write a summary of the files to exclude
    print("list of tests to exclude:")
    for idx, ex in enumerate(exclude):
        print("    {}: {}".format(idx + 1, ex))

    # build list of directories with valid example files
    if exdir is not None:
        dirs = [
            d for d in os.listdir(exdir) if "ex-" in d and d not in exclude
        ]
        # sort in numerical order for case sensitive os
        dirs = sorted(dirs, key=lambda v: (v.upper(), v[0].islower()))
    else:
        dirs = []

    # determine if only a selection of models should be run
    select_dirs = None
    select_packages = None
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "--sim":
            if len(sys.argv) > idx + 1:
                select_dirs = sys.argv[idx + 1 :]
                break
        elif arg.lower() == "--pak":
            if len(sys.argv) > idx + 1:
                select_packages = sys.argv[idx + 1 :]
                select_packages = [item.upper() for item in select_packages]
                break

    # determine if the selection of model is in the test models to evaluate
    if select_dirs is not None:
        dirs = get_select_dirs(select_dirs, dirs)
        if len(dirs) < 1:
            msg = "Selected models not available in test"
            print(msg)

    # determine if the specified package(s) is in the test models to evaluate
    if select_packages is not None:
        dirs = get_select_packages(select_packages, exdir, dirs)
        if len(dirs) < 1:
            msg = "Selected packages not available ["
            for pak in select_packages:
                msg += " {}".format(pak)
            msg += "]"
            print(msg)

    return dirs


def run_mf6(sim):
    """
    Run the MODFLOW 6 simulation and compare to existing head file or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    print(os.getcwd())
    src = os.path.join(exdir, sim.name)
    dst = os.path.join("temp", sim.name)
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
        yield run_mf6, Simulation(dir, mf6_regression=True, cmp_verbose=False)

    return


def dir_avail():
    avail = False
    if exdir is not None:
        avail = os.path.isdir(exdir)
    if not avail:
        print('"{}" does not exist'.format(exdir))
        print("no need to run {}".format(os.path.basename(__file__)))
    return avail


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = "Running {} test".format(tnam)
    print(msg)

    # determine if largetest directory exists
    dirtest = dir_avail()
    if not dirtest:
        return

    # get a list of test models to run
    dirs = get_mf6_models()

    # run the test models
    for dir in dirs:
        sim = Simulation(dir, mf6_regression=True, cmp_verbose=False)
        run_mf6(sim)

    return


if __name__ == "__main__":

    print("standalone run of {}".format(os.path.basename(__file__)))

    delFiles = True
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "--keep":
            if len(sys.argv) > idx + 1:
                delFiles = False
                break

    # run main routine
    main()
