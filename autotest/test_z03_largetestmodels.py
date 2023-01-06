import pytest

from conftest import should_compare
from simulation import Simulation


excluded = []
comparisons = {
    "test1004_mvlake_laksfr_tr": ("6.2.2",),
    "test1004_mvlake_lak_tr": ("6.2.1",),
    "test1003_MNW2_Fig28": ("6.2.1",),
    "test1001_Peterson": ("6.2.1",),
}


def test_model(function_tmpdir, large_test_model, targets, original_regression):
    exdir = large_test_model.parent
    name = exdir.name

    if name in excluded:
        pytest.skip(f"Excluding large mf6 model: {name}")

    sim = Simulation(
        name=name,
        mf6_regression=not original_regression,
        cmp_verbose=False,
        make_comparison=should_compare(name, comparisons, targets),
        simpath=str(exdir)
    )

    src = sim.simpath
    dst = str(function_tmpdir)

    # Run the MODFLOW 6 simulation and compare to existing head file or
    # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
    sim.setup(src, dst)
    sim.run()
    sim.compare()
