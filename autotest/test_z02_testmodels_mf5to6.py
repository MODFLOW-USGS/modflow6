import os

import flopy
import pytest
from common_regression import get_namefiles, model_setup
from conftest import should_compare
from framework import TestFramework

sfmt = "{:25s} - {}"
excluded_models = ["alt_model", "mf2005"]
excluded_comparisons = {
    "testPr2": ["6.2.1",],
    "testUzfLakSfr": ["6.2.1",],
    "testUzfLakSfr_laketable": ["6.2.1",],
    "testWetDry": ["6.2.1",],
}


@pytest.mark.repo
@pytest.mark.regression
def test_model(
    function_tmpdir, test_model_mf5to6, targets, original_regression
):
    exdir = test_model_mf5to6.parent
    name = exdir.name

    if name in excluded_models:
        pytest.skip(f"Excluding mf5to6 model: {name}")

    test = TestFramework(
        name=exdir.name,
        workspace=exdir,
        targets=targets,
        mf6_regression=not original_regression,
        cmp_verbose=False,
        make_comparison=should_compare(name, excluded_comparisons, targets),
    )

    src = test.workspace
    dst = function_tmpdir

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
        model_setup(lgrpth, dst)
    # copy MODFLOW-2005, MODFLOW-NWT, or MODFLOW-USG files to working directory
    else:
        npths = get_namefiles(src)
        if len(npths) < 1:
            msg = f"No name files in {src}"
            print(msg)
            assert False
        npth = npths[0]
        model_setup(npth, dst)

    # run the mf5to6 converter
    exe = os.path.abspath(targets["mf5to6"])
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

    # model setup
    src = dst
    dst = function_tmpdir / "models"
    test.setup(src, dst)

    # Run the MODFLOW 6 simulation and compare to existing head file or
    # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
    test.run()
    test.compare()
