import pytest

from conftest import should_compare
from simulation import TestSimulation

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
    exdir = large_test_model.parent
    name = exdir.name

    if name in excluded_models:
        pytest.skip(f"Excluding large mf6 model '{name}'")

    if "dev" in name and "not developmode" in markers:
        pytest.skip(f"Skipping large mf6 model '{name}' (develop mode only)")

    sim = TestSimulation(
        name=name,
        exe_dict=targets,
        mf6_regression=not original_regression,
        cmp_verbose=False,
        make_comparison=should_compare(name, excluded_comparisons, targets),
        simpath=str(exdir),
    )

    src = sim.simpath
    dst = str(function_tmpdir)

    # Run the MODFLOW 6 simulation and compare to existing head file or
    # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
    sim.setup(src, dst)
    sim.run()
    sim.compare()
