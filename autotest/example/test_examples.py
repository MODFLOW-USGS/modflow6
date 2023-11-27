import pytest
from conftest import should_compare
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
excluded_comparisons = {
    "ex-gwf-capture": ["6.2.1"],
    "ex-gwf-sagehen": ["6.2.1"],
    "ex-gwf-sfr-p01b": ["6.2.1"],
    "ex-gwf-nwt-p02a": ["6.2.1"],
    "ex-gwf-lak-p01": ["6.2.1"],
    "ex-gwf-lak-p02": ["6.2.1"],
    "ex-gwf-nwt-p02b": ["6.2.1"],
    "ex-gwf-advtidal": ["6.2.1"],
    "ex-gwf-sfr-p01": ["6.2.1"],
    "ex-gwf-lgr": ["6.2.2"],
    "ex-gwt-rotate": ["6.2.2"],
    "ex-gwt-gwtgwt-mt3dms-p10": ["6.3.0"],
    "ex-gwf-lak-p01": ["6.4.1"],
}


@pytest.mark.large
@pytest.mark.repo
@pytest.mark.regression
@pytest.mark.slow
def test_scenario(function_tmpdir, example_scenario, targets):
    name, namefiles = example_scenario
    exdirs = [nf.parent for nf in namefiles]

    if name in excluded_models:
        pytest.skip(f"Excluding mf6 model: {name}")

    for exdir in exdirs:
        model_name = f"{name}_{exdir.name}"
        workspace = function_tmpdir / model_name
        test = TestFramework(
            name=model_name,
            workspace=exdir,
            targets=targets,
            mf6_regression=True,
            cmp_verbose=False,
            make_comparison=should_compare(
                name, excluded_comparisons, targets
            ),
        )

        # Run the MODFLOW 6 simulation and compare to existing head file or
        # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
        test.setup(test.workspace, workspace)
        test.run()
        test.compare_output(test.action)
