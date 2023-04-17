import os

import flopy
import numpy as np
from flopy.utils.gridutil import get_disu_kwargs


def test_disu_simple(tmpdir, targets):
    mf6 = targets["mf6"]
    name = "disu01a"
    nlay = 3
    nrow = 3
    ncol = 3
    delr = 10.0 * np.ones(ncol)
    delc = 10.0 * np.ones(nrow)
    top = 0
    botm = [-10, -20, -30]
    disukwargs = get_disu_kwargs(nlay, nrow, ncol, delr, delc, top, botm)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6, sim_ws=str(tmpdir)
    )
    tdis = flopy.mf6.ModflowTdis(sim)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)
    ims = flopy.mf6.ModflowIms(sim, print_option="SUMMARY")
    disu = flopy.mf6.ModflowGwfdisu(gwf, **disukwargs)
    ic = flopy.mf6.ModflowGwfic(gwf, strt=0.0)
    npf = flopy.mf6.ModflowGwfnpf(gwf)
    spd = {0: [[(0,), 1.0], [(nrow * ncol - 1,), 0.0]]}
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf, stress_period_data=spd)
    sim.write_simulation()
    sim.run_simulation()


def test_disu_idomain_simple(tmpdir, targets):
    mf6 = targets["mf6"]
    name = "disu01b"
    nlay = 3
    nrow = 3
    ncol = 3
    delr = 10.0 * np.ones(ncol)
    delc = 10.0 * np.ones(nrow)
    top = 0
    botm = [-10, -20, -30]
    idomain = np.ones(nlay * nrow * ncol, dtype=int)
    idomain[1] = 0
    disukwargs = get_disu_kwargs(nlay, nrow, ncol, delr, delc, top, botm)
    disukwargs["idomain"] = idomain
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6, sim_ws=str(tmpdir)
    )
    tdis = flopy.mf6.ModflowTdis(sim)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)
    ims = flopy.mf6.ModflowIms(sim, print_option="SUMMARY")
    disu = flopy.mf6.ModflowGwfdisu(gwf, **disukwargs)
    ic = flopy.mf6.ModflowGwfic(gwf, strt=0.0)
    npf = flopy.mf6.ModflowGwfnpf(gwf)
    spd = {0: [[(0,), 1.0], [(nrow * ncol - 1,), 0.0]]}
    chd = flopy.mf6.modflow.ModflowGwfchd(gwf, stress_period_data=spd)
    oc = flopy.mf6.modflow.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.bud",
        head_filerecord=f"{name}.hds",
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )
    sim.write_simulation()
    sim.run_simulation()

    # check binary grid file
    fname = os.path.join(str(tmpdir), name + ".disu.grb")
    grbobj = flopy.mf6.utils.MfGrdFile(fname)
    nodes = grbobj._datadict["NODES"]
    ia = grbobj._datadict["IA"]
    ja = grbobj._datadict["JA"]
    assert nodes == disukwargs["nodes"]
    assert np.array_equal(ia[0:4], np.array([1, 4, 4, 7]))
    assert np.array_equal(ja[:6], np.array([1, 4, 10, 3, 6, 12]))
    assert ia[-1] == 127
    assert ia.shape[0] == 28, "ia should have size of 28"
    assert ja.shape[0] == 126, "ja should have size of 126"

    # load head array and ensure nodata value in second cell
    fname = os.path.join(str(tmpdir), name + ".hds")
    hdsobj = flopy.utils.HeadFile(fname)
    head = hdsobj.get_alldata().flatten()
    assert head[1] == 1.0e30

    # load flowja to make sure it is the right size
    fname = os.path.join(str(tmpdir), name + ".bud")
    budobj = flopy.utils.CellBudgetFile(fname, precision="double")
    flowja = budobj.get_data(text="FLOW-JA-FACE")[0].flatten()
    assert flowja.shape[0] == 126
