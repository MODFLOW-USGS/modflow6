import pytest

from conftest import should_compare
from framework import TestFramework

excluded_models = []
excluded_comparisons = {
    "test1004_mvlake_laksfr_tr": [
        "6.4.1",
    ],
    "test1004_mvlake_lak_tr": [
        "6.4.1",
    ],
    "test1003_MNW2_Fig28": [
        "6.2.1",
    ],
    "test1001_Peterson": [
        "6.2.1",
    ],
}


@pytest.mark.large
@pytest.mark.repo
@pytest.mark.regression
@pytest.mark.slow
def test_model(
    function_tmpdir, large_test_model, targets, original_regression, markers
):
    model_path = large_test_model.parent
    model_name = model_path.name

    if model_name in excluded_models:
        pytest.skip(f"Excluding large mf6 model '{model_name}'")

    if "dev" in model_name and "not developmode" in markers:
        pytest.skip(
            f"Skipping large mf6 model '{model_name}' (develop mode only)"
        )

    test = TestFramework(
        name=model_name,
        workspace=model_path,
        targets=targets,
        compare="compare"
        if original_regression
        else "mf6_regression"
        if should_compare(model_name, excluded_comparisons, targets)
        else None,
        verbose=False,
    )

    # setup temp dir as test workspace
    test.setup(model_path, function_tmpdir)

    # Run the MODFLOW 6 simulation and compare to results generated using
    # 1) the current MODFLOW 6 release, 2) an existing head file, or 3) or
    # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
    test.run()
