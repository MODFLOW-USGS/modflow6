import os
import sys

import pytest

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

exdir = os.path.join("..", "tmp_simulations")
testpaths = os.path.join("..", exdir)


def dir_avail():
    avail = os.path.isdir(exdir)
    if not avail:
        print(f'"{exdir}" does not exist')
        print(f"no need to run {os.path.basename(__file__)}")
    return avail


def get_mf6_models():
    """
    Get a list of test models
    """

    # determine if test directory exists
    dirtest = dir_avail()
    if not dirtest:
        return []

    # tuple of example files to exclude
    exclude = ("test006_03models", "test018_NAC", "test051_uzf1d_a")

    exclude_continuous_integration = (
        "test006_gwf3_transport",
        "test006_Gwf1-Lnf1",
    )

    # build list of directories with valid example files
    exclude = list(exclude + exclude_continuous_integration)
    dirs = [d for d in os.listdir(exdir) if "test" in d and d not in exclude]
    # sort in numerical order for case sensitive os
    dirs = sorted(dirs, key=lambda v: (v.upper(), v[0].islower()))

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
        found_dirs = []
        for d in select_dirs:
            if d in dirs:
                found_dirs.append(d)
        dirs = found_dirs
        if len(dirs) < 1:
            msg = "Selected models not available in test"
            print(msg)

    # determine if the specified package(s) is in the test models to evaluate
    if select_packages is not None:
        found_dirs = []
        for d in dirs:
            pth = os.path.join(exdir, d)
            namefiles = pymake.get_namefiles(pth)
            ftypes = []
            for namefile in namefiles:
                ftype = pymake.autotest.get_mf6_ftypes(
                    namefile, select_packages
                )
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
            msg = "Selected packages not available ["
            for pak in select_packages:
                msg += f" {pak}"
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


@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(get_mf6_models())),
)
def test_mf6model(idx, dir):
    # run the test model
    run_mf6(Simulation(dir))


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = f"Running {tnam} test"
    print(msg)

    # get a list of test models to run
    dirs = get_mf6_models()

    # run the test model
    for dir in dirs:
        sim = Simulation(dir)
        run_mf6(sim)

    return


if __name__ == "__main__":

    print(f"standalone run of {os.path.basename(__file__)}")

    delFiles = True
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "--keep":
            if len(sys.argv) > idx + 1:
                delFiles = False
                break

    # run main routine
    main()
