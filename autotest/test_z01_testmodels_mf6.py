import os
import subprocess
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

from common_regression import (
    get_example_basedir,
    get_example_dirs,
    get_home_dir,
    get_select_dirs,
    get_select_packages,
    is_directory_available,
    set_mf6_regression,
)
from simulation import Simulation
from targets import get_mf6_version

# find path to examples directory
home = get_home_dir()

find_dir = "modflow6-testmodels"
example_basedir = get_example_basedir(home, find_dir, subdir="mf6")

if example_basedir is not None:
    assert os.path.isdir(example_basedir)


def get_branch():
    try:
        # determine current buildstat branch
        b = subprocess.Popen(
            ("git", "status"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT
        ).communicate()[0]
        if isinstance(b, bytes):
            b = b.decode("utf-8")

        # determine current buildstat branch
        for line in b.splitlines():
            if "On branch" in line:
                branch = line.replace("On branch ", "").rstrip()
    except:
        branch = None

    return branch


def get_mf6_models():
    """
    Get a list of test models
    """

    # determine if test directory exists
    dir_avail = is_directory_available(example_basedir)
    if not dir_avail:
        return []

    # determine if running on travis
    is_CI = "CI" in os.environ

    # get current branch
    if is_CI:
        branch = os.path.basename(os.environ["GITHUB_REF"])
    else:
        branch = get_branch()
    print(f"On branch {branch}")

    # tuple of example files to exclude
    # exclude = (None,)
    exclude = ("test205_gwtbuy-henrytidal",)

    # update exclude
    if is_CI:
        exclude_CI = (None,)
        exclude = exclude + exclude_CI
    exclude = list(exclude)

    # write a summary of the files to exclude
    print("list of tests to exclude:")
    for idx, ex in enumerate(exclude):
        print(f"    {idx + 1}: {ex}")

    # build list of directories with valid example files
    if example_basedir is not None:
        example_dirs = get_example_dirs(
            example_basedir, exclude, prefix="test"
        )
    else:
        example_dirs = []

    # exclude dev examples on master or release branches
    if "master" in branch.lower() or "release" in branch.lower() or branch.lower().startswith("v6"):
        drmv = []
        for d in example_dirs:
            if "_dev" in d.lower():
                drmv.append(d)
        for d in drmv:
            example_dirs.remove(d)

    # determine if only a selection of models should be run
    select_example_dirs = None
    select_packages = None
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "--sim":
            if len(sys.argv) > idx + 1:
                select_example_dirs = sys.argv[idx + 1 :]
                break
        elif arg.lower() == "--pak":
            if len(sys.argv) > idx + 1:
                select_packages = sys.argv[idx + 1 :]
                select_packages = [item.upper() for item in select_packages]
                break
        elif arg.lower() == "--match":
            if len(sys.argv) > idx + 1:
                like = sys.argv[idx + 1]
                example_dirs = [item for item in example_dirs if like in item]
                break

    # determine if the selection of model is in the test models to evaluate
    if select_example_dirs is not None:
        example_dirs = get_select_dirs(select_example_dirs, example_dirs)
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
            for pak in select_packages:
                msg += f" {pak}"
            msg += "]"
            print(msg)

    return example_dirs


def run_mf6(sim):
    """
    Run the MODFLOW 6 simulation and compare to results generated using
    1) the current MODFLOW 6 release, 2) an existing head file, or 3) or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    print("Current working directory: ".format(os.getcwd()))
    src = os.path.join(example_basedir, sim.name)
    dst = os.path.join("temp", sim.name)
    sim.setup(src, dst)
    sim.run()
    sim.compare()
    sim.teardown()


def set_make_comparison(test):
    compare_tests = {
        "test001e_noUZF_3lay": ("6.2.1",),
        "test005_advgw_tidal": ("6.2.1",),
        "test017_Crinkle": ("6.2.1",),
        "test028_sfr": ("6.2.1",),
        "test028_sfr_rewet": ("6.2.1",),
        "test028_sfr_rewet_nr": ("6.2.1",),
        "test028_sfr_rewet_simple": ("6.2.1",),
        "test028_sfr_simple": ("6.2.1",),
        "test034_nwtp2": ("6.2.1",),
        "test034_nwtp2_1d": ("6.2.1",),
        "test045_lake1tr_nr": ("6.2.1",),
        "test045_lake2tr": ("6.2.1",),
        "test045_lake2tr_nr": ("6.2.1",),
        "test051_uzfp2": ("6.2.1",),
        "test051_uzfp3_lakmvr_v2": ("6.2.1",),
        "test051_uzfp3_wellakmvr_v2": ("6.2.1",),
        "test045_lake4ss": ("6.2.2",),
        "test056_mt3dms_usgs_gwtex_dev": ("6.2.2",),
        "test056_mt3dms_usgs_gwtex_IR_dev": ("6.2.2",),
    }
    make_comparison = True
    if test in compare_tests.keys():
        version = get_mf6_version()
        print(f"MODFLOW version='{version}'")
        version = get_mf6_version(version="mf6-regression")
        print(f"MODFLOW regression version='{version}'")
        if version in compare_tests[test]:
            make_comparison = False
    return make_comparison


mf6_models = get_mf6_models()


@pytest.mark.parametrize(
    "exdir",
    mf6_models,
)
def test_mf6model(exdir):
    # run the test model
    run_mf6(
        Simulation(
            exdir,
            mf6_regression=set_mf6_regression(),
            cmp_verbose=False,
            make_comparison=set_make_comparison(exdir),
        )
    )


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = f"Running {tnam} test"
    print(msg)

    # determine if test directory exists
    dir_available = is_directory_available(example_basedir)
    if not dir_available:
        return

    # get a list of test models to run
    example_dirs = get_mf6_models()

    # run the test model
    for on_dir in example_dirs:
        sim = Simulation(
            on_dir,
            mf6_regression=set_mf6_regression(),
            cmp_verbose=True,
            make_comparison=set_make_comparison(on_dir),
        )
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
