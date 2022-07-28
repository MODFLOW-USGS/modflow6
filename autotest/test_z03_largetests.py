import os
import sys

import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

try:
    from modflow_devtools import (
        get_example_basedir,
        get_example_dirs,
        get_home_dir,
        get_select_dirs,
        get_select_packages,
        is_directory_available,
        Simulation,
        get_mf6_version,
        set_mf6_regression,
    )
except:
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

home = get_home_dir()

find_dir = "modflow6-largetestmodels"
example_basedir = get_example_basedir(home, find_dir)


def get_mf6_models():
    """
    Get a list of test models
    """

    # determine if largetest directory exists
    dir_available = is_directory_available(example_basedir)
    if not dir_available:
        return []

    # determine if running on CI
    is_CI = "CI" in os.environ

    # tuple of example files to exclude
    exclude = (None,)

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
            for pak in select_packages:
                msg += f" {pak}"
            msg += "]"
            print(msg)

    return example_dirs


def run_mf6(sim):
    """
    Run the MODFLOW 6 simulation and compare to existing head file or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    print(os.getcwd())
    src = os.path.join(example_basedir, sim.name)
    dst = os.path.join("temp", sim.name)
    sim.setup(src, dst)
    sim.run()
    sim.compare()
    sim.teardown()


def set_make_comparison(test, context=None):
    compare_tests = {
        "test1004_mvlake_laksfr_tr": ("6.2.2",),
        "test1004_mvlake_lak_tr": ("6.2.1",),
        "test1003_MNW2_Fig28": ("6.2.1",),
        "test1001_Peterson": ("6.2.1",),
    }
    make_comparison = True
    if test in compare_tests.keys():
        if context:
            version = context.get_mf6_version()
            regression_version = context.get_mf6_version(version="mf6-regression")
        else:
            version = get_mf6_version()
            regression_version = get_mf6_version(version="mf6-regression")
        print(f"MODFLOW version='{version}'")
        print(f"MODFLOW regression version='{regression_version}'")
        if version in compare_tests[test]:
            make_comparison = False
    return make_comparison


mf6_models = get_mf6_models()


@pytest.mark.parametrize(
    "exdir",
    mf6_models,
)
def test_mf6model(exdir, mf6testctx):
    # run the test model

    exe_dict=None
    if mf6testctx:
        exe_dict = mf6testctx.get_target_dictionary()

    run_mf6(
        Simulation(
            exdir,
            exe_dict=exe_dict,
            mf6_regression=set_mf6_regression(),
            cmp_verbose=False,
            make_comparison=set_make_comparison(exdir, context=mf6testctx),
        )
    )


def main():
    # write message
    tnam = os.path.splitext(os.path.basename(__file__))[0]
    msg = f"Running {tnam} test"
    print(msg)

    # get a list of test models to run
    example_dirs = get_mf6_models()

    # run the test model
    for on_dir in example_dirs:
        sim = Simulation(
            on_dir,
            mf6_regression=set_mf6_regression(),
            cmp_verbose=False,
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
