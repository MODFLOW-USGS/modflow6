import os
import sys
import pathlib

import time
import shutil

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

from targets import target_dict as target_dict
from common_regression import (
    get_home_dir,
    get_example_basedir,
    is_directory_available,
    get_example_dirs,
    get_select_dirs,
    get_select_packages,
    set_mf6_regression,
)


# find path to examples directory
home = get_home_dir()

find_dir = "modflow6-testmodels"
example_basedir = get_example_basedir(home, find_dir, subdir="mf5to6")

if example_basedir is not None:
    assert os.path.isdir(example_basedir)

sfmt = "{:25s} - {}"


def get_mf5to6_models():
    """
    Get a list of test models
    """
    # list of example files to exclude
    exclude = (None,)

    # write a summary of the files to exclude
    print("list of tests to exclude:")
    for idx, ex in enumerate(exclude):
        print("    {}: {}".format(idx + 1, ex))

    # build list of directories with valid example files
    if example_basedir is not None:
        example_dirs = get_example_dirs(
            example_basedir,
            exclude,
            prefix="test",
            find_sim=False,
        )
    else:
        example_dirs = []

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
        example_dirs = get_select_dirs(select_dirs, example_dirs)
        if len(example_dirs) < 1:
            msg = "Selected models not available in test"
            print(msg)

    # determine if the specified package(s) is in the test models to evaluate
    if select_packages is not None:
        example_dirs = get_select_packages(
            select_packages, example_basedir, example_dirs
        )
        if len(example_dirs) < 1:
            msg = "Selected packages not available ["
            for idx, pak in enumerate(select_packages):
                msg += "{}".format(pak)
                if idx + 1 < len(select_packages):
                    msg += ", "
            msg += "]"
            print(msg)

    return example_dirs


def run_mf5to6(sim):
    """
    Run the MODFLOW 6 simulation and compare to existing head file or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    src = os.path.join(example_basedir, sim.name)
    dst = os.path.join("temp", "working")

    # set lgrpth to None
    lgrpth = None

    # determine if compare directory exists in directory or if mflgr control
    # file is in directory
    listdir = os.listdir(src)
    for value in listdir:
        fpth = os.path.join(src, value)
        if os.path.isfile(fpth):
            ext = os.path.splitext(fpth)[1]
            if ".lgr" in ext.lower():
                lgrpth = fpth

    print("Copying files to working directory")
    # copy lgr files to working directory
    if lgrpth is not None:
        npth = lgrpth
        pymake.setup(lgrpth, dst)
    # copy MODFLOW-2005, MODFLOW-NWT, or MODFLOW-USG files to working directory
    else:
        npths = pymake.get_namefiles(src)
        if len(npths) < 1:
            msg = "No name files in {}".format(src)
            print(msg)
            assert False
        npth = npths[0]
        pymake.setup(npth, dst)

    # run the mf5to6 converter
    exe = os.path.abspath(target_dict["mf5to6"])
    print(sfmt.format("using executable", exe))
    nmsg = "Program terminated normally"
    try:
        nam = os.path.basename(npth)
        success, buff = flopy.run_model(
            exe,
            nam,
            model_ws=dst,
            silent=False,
            report=True,
            normal_msg=nmsg,
            cargs="mf6",
        )
        msg = sfmt.format("MODFLOW 5 to 6 run", nam)
        if success:
            print(msg)
        else:
            print("ERROR: " + msg)
    except:
        msg = sfmt.format("MODFLOW 5 to 6 run", nam)
        print("ERROR: " + msg)
        success = False

    assert success, msg

    # standard setup
    src = dst
    dst = os.path.join("temp", sim.name)
    sim.setup(src, dst)

    # clean up temp/working directory (src)
    if os.path.exists(src):
        msg = "Removing {} directory".format(src)
        print(msg)
        shutil.rmtree(src)
        time.sleep(0.5)

    # standard comparison run
    sim.run()
    sim.compare()
    sim.teardown()


def test_model():
    # determine if test directory exists
    dir_available = is_directory_available(example_basedir)
    if not dir_available:
        return

    # get a list of test models to run
    example_dirs = get_mf5to6_models()

    # run the test models
    for on_dir in example_dirs:
        yield run_mf5to6, Simulation(
            on_dir, mf6_regression=set_mf6_regression(), cmp_verbose=False
        )

    return


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = "Running {} test".format(tnam)
    print(msg)

    # get name of current file
    module_name = sys.modules[__name__].__file__

    # determine if test directory exists
    dir_available = is_directory_available(example_basedir)
    if not dir_available:
        return

    # get a list of test models to run
    example_dirs = get_mf5to6_models()

    # run the test models
    for on_dir in example_dirs:
        sim = Simulation(
            on_dir, mf6_regression=set_mf6_regression(), cmp_verbose=False
        )
        run_mf5to6(sim)

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
