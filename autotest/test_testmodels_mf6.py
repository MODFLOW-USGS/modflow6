from shutil import copytree

import pytest

from framework import TestFramework
from common_regression import (
    setup_mf6,
    setup_mf6_comparison,
    get_mf6_comparison,
)

excluded_models = [
    "alt_model",
    "test205_gwtbuy-henrytidal",
    # todo reinstate when flopy fixed: https://github.com/modflowpy/flopy/issues/2053
    "test001a_Tharmonic_tabs",
    "test004_bcfss",
    "test014_NWTP3Low_dev",
    "test041_flowdivert_nwt_dev",
    # remove tests with nwt usg conductance weighting 
    "test006_gwf3_gnc_nr_dev",
    "test006_gwf3_nr_dev",
    "test014_NWTP3High_dev",
    "test015_KeatingLike_disu_dev",
    "test041_flowdivert_nr_dev",
    "test016_Keating_disu_dev",
    "test053_npf-a-nwt_dev",
    "test053_npf-b-nwt_dev",
]


@pytest.mark.repo
@pytest.mark.regression
def test_model(
    function_tmpdir,
    markers,
    original_regression,
    targets,
    # https://modflow-devtools.readthedocs.io/en/latest/md/fixtures.html#modflow-6-test-models
    test_model_mf6,
):
    model_path = test_model_mf6.parent
    model_name = model_path.name
    excluded = model_name in excluded_models
    compare = get_mf6_comparison(model_path) if original_regression else "mf6_regression"
    dev_only = "dev" in model_name and "not developmode" in markers
    if excluded or dev_only:
        reason = "excluded" if excluded else "developmode only"
        pytest.skip(f"Skipping: {model_name} ({reason})")

    # setup test workspace and framework
    setup_mf6(src=model_path, dst=function_tmpdir)
    test = TestFramework(
        name=model_name,
        workspace=function_tmpdir,
        targets=targets,
        compare=compare,
        verbose=False,
    )

    # setup comparison workspace
    if compare == "mf6_regression":
        copytree(function_tmpdir, function_tmpdir / compare)
    else:
        setup_mf6_comparison(
            model_path, function_tmpdir, compare, overwrite=True
        )

    # run the test
    test.run()
