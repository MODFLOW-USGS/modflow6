import os
import sys

import pytest
from common_regression import get_mf6_ftypes, get_namefiles
from framework import TestFramework

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
            namefiles = get_namefiles(pth)
            ftypes = []
            for namefile in namefiles:
                ftype = get_mf6_ftypes(namefile, select_packages)
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


def run_mf6(sim, ws):
    """
    Run the MODFLOW 6 simulation and compare to existing head file or
    appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.

    """
    src = os.path.join(exdir, sim.name)
    dst = os.path.join(ws, sim.name)
    sim.setup(src, dst)
    sim.run()
    sim.compare()


@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(get_mf6_models())),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
    )
    run_mf6(test, function_tmpdir)
