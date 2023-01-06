import pytest

from conftest import should_compare
from simulation import Simulation


excluded = ["ex-gwf-csub-p02c"]
comparisons = {
    "ex-gwf-capture": ("6.2.1",),
    "ex-gwf-sagehen": ("6.2.1",),
    "ex-gwf-sfr-p01b": ("6.2.1",),
    "ex-gwf-nwt-p02a": ("6.2.1",),
    "ex-gwf-lak-p01": ("6.2.1",),
    "ex-gwf-lak-p02": ("6.2.1",),
    "ex-gwf-nwt-p02b": ("6.2.1",),
    "ex-gwf-advtidal": ("6.2.1",),
    "ex-gwf-sfr-p01": ("6.2.1",),
    "ex-gwf-lgr": ("6.2.2",),
    "ex-gwt-rotate": ("6.2.2",),
    "ex-gwt-gwtgwt-mt3dms-p10": ("6.3.0",),
}


def test_scenario(function_tmpdir, example_scenario, targets):
    name, namefiles = example_scenario
    exdirs = [nf.parent for nf in namefiles]

    if name in excluded:
        pytest.skip(f"Excluding mf6 model: {name}")

    for exdir in exdirs:
        model_name = f"{name}_{exdir.name}"
        if exdir.name in ["mf6gwt"]:
            pytest.skip(f"Skipping coupled GWT model: {name}/{exdir.name}")

        workspace = function_tmpdir / model_name
        sim = Simulation(
            name=model_name,
            mf6_regression=True,
            cmp_verbose=False,
            make_comparison=should_compare(name, comparisons, targets),
            simpath=str(exdir)
        )

        src = sim.simpath
        dst = str(workspace)

        # Run the MODFLOW 6 simulation and compare to existing head file or
        # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
        sim.setup(src, dst)
        sim.run()
        sim.compare()
        sim.teardown()
