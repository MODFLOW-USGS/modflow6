import pytest

from framework import TestFramework

excluded_models = []


@pytest.mark.large
@pytest.mark.repo
@pytest.mark.regression
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
