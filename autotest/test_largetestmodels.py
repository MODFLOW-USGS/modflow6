from shutil import copytree

import pytest
from common_regression import get_mf6_comparison, setup_mf6, setup_mf6_comparison
from framework import TestFramework

excluded_models = [
    "test1002_biscqtg_disv_gnc_nr_dev",
    "test1002_biscqtg_disv_nr_MD_dev",
    "test1002_biscqtg_disv_nr_RCM_dev",
    "test1002_biscqtg_disv_nr_dev",
]


@pytest.mark.regression
@pytest.mark.repo
@pytest.mark.slow
def test_model(
    function_tmpdir,
    # https://modflow-devtools.readthedocs.io/en/latest/md/fixtures.html#large-test-models
    large_test_model,
    markers,
    original_regression,
    targets,
):
    model_path = large_test_model.parent
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
