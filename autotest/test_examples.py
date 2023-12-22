import pytest

from framework import TestFramework

# skip nested models
# ex-gwf-csub-p02c has subdirs like 'es-001', 'hb-100'
# all others just have 2 folders 'mf6gwf' and 'mf6gwt'
excluded_models = [
    "ex-gwf-csub-p02c",
    "ex-gwt-hecht-mendez-b",
    "ex-gwt-hecht-mendez-c",
    "ex-gwt-keating",
    "ex-gwt-moc3d-p01a",
    "ex-gwt-moc3d-p01b",
    "ex-gwt-moc3d-p01c",
    "ex-gwt-moc3d-p01d",
    "ex-gwt-moc3d-p02",
    "ex-gwt-moc3d-p02tg",
    "ex-gwt-mt3dms-p02a",
    "ex-gwt-mt3dms-p02b",
    "ex-gwt-mt3dms-p02c",
    "ex-gwt-mt3dms-p02d",
    "ex-gwt-mt3dms-p02e",
    "ex-gwt-mt3dms-p02f",
    "ex-gwt-mt3dsupp631",
    "ex-gwt-mt3dsupp632a",
    "ex-gwt-mt3dsupp632b",
    "ex-gwt-mt3dsupp632c",
    "ex-gwt-mt3dsupp82",
    "ex-gwt-prudic2004t2",
]


@pytest.mark.large
@pytest.mark.repo
@pytest.mark.regression
@pytest.mark.slow
def test_scenario(
    # https://modflow-devtools.readthedocs.io/en/latest/md/fixtures.html#example-scenarios
    function_tmpdir,
    example_scenario,
    targets,
):
    name, namefiles = example_scenario
    if name in excluded_models:
        pytest.skip(f"Skipping: {name} (excluded)")

    model_paths = [nf.parent for nf in namefiles]
    for model_path in model_paths:
        model_name = f"{name}_{model_path.name}"
        workspace = function_tmpdir / model_name
        test = TestFramework(
            name=model_name,
            workspace=model_path,
            targets=targets,
            compare="mf6_regression",
            verbose=False,
        )
        test.setup(model_path, workspace)
        test.run()
