from shutil import copytree

import pytest
from common_regression import get_mf6_comparison, setup_mf6, setup_mf6_comparison
from framework import TestFramework

excluded_models = [
    "alt_model",
    "test205_gwtbuy-henrytidal",
    # todo reinstate after 6.5.0 release
    "test001d_Tnewton",
    # remove tests with nwt usg conductance weighting
    "test006_gwf3_gnc_nr_dev",
    "test006_gwf3_nr_dev",
    "test014_NWTP3High_dev",
    "test015_KeatingLike_disu_dev",
    "test041_flowdivert_nr_dev",
    "test016_Keating_disu_dev",
    "test053_npf-a-nwt_dev",
    "test053_npf-b-nwt_dev",
    # todo reinstate after resolving convergence failure
    "test014_NWTP3Low_dev",
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
    compare = (
        get_mf6_comparison(model_path) if original_regression else "mf6_regression"
    )
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
        setup_mf6_comparison(model_path, function_tmpdir, compare, overwrite=True)

    # run the test
    test.run()
