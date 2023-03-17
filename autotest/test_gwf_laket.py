# Test for checking lak evaporation.


import os
import shutil
import sys

import flopy
import numpy as np
import pytest
from framework import TestFramework
from simulation import TestSimulation

ex = [
    "gwf_laket",
]


def get_model(idx, ws):
    name = ex[idx]
    nlay = 1
    nrow = 1
    ncol = 1
    nper = 2
    delc = 1.0
    delr = 1.0
    delz = 10.0
    top = 5.0
    botm = [top - (k + 1) * delz for k in range(nlay)]

    perlen = [11.0, 11.0]
    nstp = [11, 11]
    tsmult = [1.0, 1.0]

    Kh = 1.0
    Kv = 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    # build MODFLOW 6 files
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )

    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="DAYS",
        nper=nper,
        perioddata=tdis_rc,
    )

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="simple",
        linear_acceleration="BICGSTAB",
    )

    # create gwf model
    gwfname = name
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, newtonoptions="NEWTON")

    # discretization package
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # initial conditions
    strt = np.zeros((nlay, nrow, ncol), dtype=float)
    strt += top
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_specific_discharge=False,
        icelltype=1,
        k=Kh,
        k33=Kv,
    )

    sy = 0.3
    ss = 1e-5
    sto = flopy.mf6.ModflowGwfsto(gwf, sy=sy, ss=ss, iconvert=1)

    lake_vconnect = [
        (0, 0, 0),
    ]
    nlakeconn = 1

    # pak_data = [lakeno, strt, nlakeconn]
    initial_stage = top + 1.0
    pak_data = [(0, initial_stage, nlakeconn)]

    bedleak = 0.0  # "None"
    belev = top
    con_data = [
        (0, i, idx, "VERTICAL", bedleak, belev, -99, -99, -99)
        for i, idx in enumerate(lake_vconnect)
    ]

    # period data
    p_data = {
        0: [
            (0, "EVAPORATION", 0.1),
        ],
        1: [
            (0, "RAINFALL", 0.2),
        ],
    }

    # note: for specifying lake number, use fortran indexing!
    fname = f"{gwfname}.lak.obs.csv"
    lak_obs = {
        fname: [
            ("lakestage", "stage", (0,)),
            ("lakevolume", "volume", (0,)),
            ("evap", "evaporation", (0,)),
        ],
    }

    lak = flopy.mf6.modflow.ModflowGwflak(
        gwf,
        surfdep=0.0,
        print_input=True,
        print_flows=True,
        print_stage=True,
        nlakes=len(pak_data),
        ntables=0,
        packagedata=pak_data,
        pname="LAK-1",
        connectiondata=con_data,
        perioddata=p_data,
        observations=lak_obs,
    )

    # chd = flopy.mf6.modflow.ModflowGwfchd(gwf, stress_period_data=[(0, 0, 0, top + 0.1),])

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        printrecord=[("BUDGET", "ALL"), ("HEAD", "ALL")],
    )

    return sim


def build_model(idx, dir):

    # build MODFLOW 6 files
    sim = get_model(idx, dir)

    return sim, None


def eval_laket(sim):
    msg = "Evaluating Lake ET. "

    fpth = os.path.join(sim.simpath, "gwf_laket.lak.obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    obs = [
        (1.000000000000, 5.9000000000000004, -0.1),
        (2.000000000000, 5.8000000000000007, -0.1),
        (3.000000000000, 5.7000000000000011, -0.1),
        (4.000000000000, 5.6000000000000014, -0.1),
        (5.000000000000, 5.5000000000000018, -0.1),
        (6.000000000000, 5.4000000000000021, -0.1),
        (7.000000000000, 5.3000000000000025, -0.1),
        (8.000000000000, 5.2000000000000028, -0.1),
        (9.000000000000, 5.1000000000000032, -0.1),
        (10.00000000000, 5.0000000000000036, -0.1),
        (11.00000000000, 5.0000000000000000, -0.35527136788005009e-14),
        (12.00000000000, 5.0999999999999996, -0.1),
        (13.00000000000, 5.1999999999999993, -0.1),
        (14.00000000000, 5.2999999999999989, -0.1),
        (15.00000000000, 5.3999999999999986, -0.1),
        (16.00000000000, 5.4999999999999982, -0.1),
        (17.00000000000, 5.5999999999999979, -0.1),
        (18.00000000000, 5.6999999999999975, -0.1),
        (19.00000000000, 5.7999999999999972, -0.1),
        (20.00000000000, 5.8999999999999968, -0.1),
        (21.00000000000, 5.9999999999999964, -0.1),
        (22.00000000000, 6.0999999999999961, -0.1),
    ]

    obs = np.array(
        obs, dtype=[("time", float), ("stage", float), ("evap", float)]
    )
    
    evap_compare = np.allclose(obs["evap"], tc["EVAP"])
    stage_compare = np.allclose(obs["stage"], tc["LAKESTAGE"])

    sim.success = True
    if not evap_compare:
        sim.success = False
        msg += f" Lake evaporation comparison failed."
    if not stage_compare:
        sim.success = False
        msg += f" Lake stage comparison failed."
    assert sim.success, msg


@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework()
    test.build(build_model, idx, str(function_tmpdir))
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_laket,
            idxsim=idx,
            mf6_regression=False,
        ),
        str(function_tmpdir),
    )

