import pytest

from framework import TestFramework

excluded_models = ["alt_model", "test205_gwtbuy-henrytidal"]


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
    dev_only = "dev" in model_name and "not developmode" in markers
    if excluded or dev_only:
        reason = "excluded" if excluded else "developmode only"
        pytest.skip(f"Skipping: {model_name} ({reason})")

    test = TestFramework(
        name=model_name,
        workspace=model_path,
        targets=targets,
        compare="auto" if original_regression else "mf6_regression",
        verbose=False,
    )
    test.setup(model_path, function_tmpdir)
    test.run()
