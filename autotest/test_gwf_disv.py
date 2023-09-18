"""
Test of GWF DISV Package.  Use the flopy disv tool to create
a simple regular grid example, but using DISV instead of DISU.
Use a large offset for x and y vertices to ensure that the area
calculation in MODFLOW 6 is correct.  For the second case, set
one of the cells inactive and test to make sure connectivity
in binary grid file is correct.

"""

import os

import flopy
import numpy as np
import pytest
from flopy.utils.gridutil import get_disv_kwargs

from framework import TestFramework
from simulation import TestSimulation

ex = ["disv01a", "disv01b"]


def build_model(idx, dir, mf6):
    name = ex[idx]
    ws = dir
    nlay = 3
    nrow = 3
    ncol = 3
    delr = 10.0
    delc = 10.0
    top = 0
    botm = [-10, -20, -30]
    xoff = 100000000.0
    yoff = 100000000.0
    disvkwargs = get_disv_kwargs(
        nlay,
        nrow,
        ncol,
        delr,
        delc,
        top,
        botm,
        xoff,
        yoff,
    )
    if idx == 1:
        # for the second test, set one cell to idomain = 0
        idomain = np.ones((nlay, nrow * ncol), dtype=int)
        idomain[0, 1] = 0
        disvkwargs["idomain"] = idomain

    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name=mf6,
        sim_ws=ws,
    )
    tdis = flopy.mf6.ModflowTdis(sim)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)
    ims = flopy.mf6.ModflowIms(sim, print_option="SUMMARY")
    disv = flopy.mf6.ModflowGwfdisv(gwf, **disvkwargs)
    ic = flopy.mf6.ModflowGwfic(gwf, strt=0.0)
    npf = flopy.mf6.ModflowGwfnpf(gwf)
    spd = {0: [[(0, 0), 1.0], [(0, nrow * ncol - 1), 0.0]]}
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf, stress_period_data=spd)
    return sim, None


def eval_results(sim):
    print("evaluating results...")

    name = sim.name

    fname = os.path.join(sim.simpath, name + ".disv.grb")
    grbobj = flopy.mf6.utils.MfGrdFile(fname)
    ncpl = grbobj._datadict["NCPL"]
    ia = grbobj._datadict["IA"]
    ja = grbobj._datadict["JA"]

    if sim.idxsim == 1:
        # assert ncpl == disvkwargs["ncpl"]
        assert np.array_equal(ia[0:4], np.array([1, 4, 4, 7]))
        assert np.array_equal(ja[:6], np.array([1, 4, 10, 3, 6, 12]))
        assert ia[-1] == 127
        assert ia.shape[0] == 28, "ia should have size of 28"
        assert ja.shape[0] == 126, "ja should have size of 126"


@pytest.mark.parametrize("idx, name", list(enumerate(ex)))
def test_mf6model(idx, name, function_tmpdir, targets):
    ws = str(function_tmpdir)
    sim, _ = build_model(idx, ws, targets.mf6)
    sim.write_simulation()
    sim.run_simulation()
    test = TestFramework()
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_results,
            idxsim=idx,
            make_comparison=False,
        ),
        ws,
    )
