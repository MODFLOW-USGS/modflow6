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
        pytest.skip(f"Skipping large mf6 model '{model_name}' (develop mode only)")

    test = TestFramework(
        name=model_name,
        workspace=model_path,
        targets=targets,
        comparison="compare" if original_regression else "mf6_regression",
        cmp_verbose=False,
        make_comparison=should_compare(model_name, excluded_comparisons, targets),
    )

    test.setup(model_path, function_tmpdir)
    test.run()
