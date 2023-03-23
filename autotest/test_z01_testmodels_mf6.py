import pytest
from conftest import should_compare
from simulation import TestSimulation

excluded_models = ["alt_model", "test205_gwtbuy-henrytidal"]
excluded_comparisons = {
    "test001e_noUZF_3lay": ("6.2.1",),
    "test005_advgw_tidal": ("6.2.1",),
    "test017_Crinkle": ("6.2.1",),
    "test028_sfr": ("6.2.1",),
    "test028_sfr_rewet": ("6.2.1",),
    "test028_sfr_rewet_nr": ("6.2.1",),
    "test028_sfr_rewet_simple": ("6.2.1",),
    "test028_sfr_simple": ("6.2.1",),
    "test034_nwtp2": ("6.2.1",),
    "test034_nwtp2_1d": ("6.2.1",),
    "test045_lake1tr_nr": ("6.2.1",),
    "test045_lake2tr": ("6.2.1",),
    "test045_lake2tr_nr": ("6.2.1",),
    "test051_uzfp2": ("6.2.1",),
    "test051_uzfp3_lakmvr_v2": ("6.2.1",),
    "test051_uzfp3_wellakmvr_v2": ("6.2.1",),
    "test045_lake4ss": ("6.2.2",),
    "test056_mt3dms_usgs_gwtex_dev": ("6.4.1",),
    "test056_mt3dms_usgs_gwtex_IR_dev": ("6.4.1",),
    "test059_mvlake_lak_ss": ("6.4.1",),
}


@pytest.mark.repo
@pytest.mark.regression
def test_model(function_tmpdir, test_model_mf6, targets, original_regression):
    exdir = test_model_mf6.parent
    name = exdir.name

    if name in excluded_models:
        pytest.skip(f"Excluding mf6 model: {name}")

    sim = TestSimulation(
        name=name,
        exe_dict=targets,
        mf6_regression=not original_regression,
        cmp_verbose=False,
        make_comparison=should_compare(name, excluded_comparisons, targets),
        simpath=str(exdir),
    )

    src = exdir
    dst = str(function_tmpdir)

    # Run the MODFLOW 6 simulation and compare to results generated using
    # 1) the current MODFLOW 6 release, 2) an existing head file, or 3) or
    # appropriate MODFLOW-2005, MODFLOW-NWT, MODFLOW-USG, or MODFLOW-LGR run.
    sim.setup(src, dst)
    sim.run()
    sim.compare()
