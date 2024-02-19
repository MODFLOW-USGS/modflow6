# test based on issue 1585 fix
import flopy
import numpy as np
import pytest

from framework import TestFramework

paktest = "sfr"
cases = ["sfr-1585"]


def build_models(idx, test):
    # static model data
    # temporal discretization
    nper = 2
    tdis_rc = []
    for _ in range(nper):
        tdis_rc.append((1.0, 1, 1.0))

    # spatial discretization data
    nlay, nrow, ncol = 1, 10, 10
    delr, delc = 100.0, 100.0
    top = 0.0
    botm = -10
    strt = 0.0

    # calculate hk
    hk = 1.0e-4

    # solver options
    hclose, rclose = 1e-9, 1e-3

    # build MODFLOW 6 files
    name = cases[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=test.workspace
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        complexity="simple",
        outer_dvclose=hclose,
        under_relaxation="NONE",
        inner_dvclose=hclose,
        rcloserecord=rclose,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
    )

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
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=hk)

    # chd files
    # chd data
    spd = [
        [(0, 0, 0), 1.0],
        [(0, nrow - 1, ncol - 1), 0.0],
    ]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf, stress_period_data=spd, pname="chd-1"
    )

    # sfr file
    packagedata = [
        [
            0,
            (-1, -1, -1),
            delr,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-5,
            3.0e-2,
            1,
            0.0,
            0,
        ],
        [
            1,
            (-1, -1, -1),
            delr,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-5,
            3.0e-2,
            2,
            1.0,
            0,
        ],
        [
            2,
            (-1, -1, -1),
            delr,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-5,
            3.0e-2,
            2,
            1.0,
            0,
        ],
        [
            3,
            (-1, -1, -1),
            delr,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-5,
            3.0e-2,
            2,
            1.0,
            0,
        ],
        [
            4,
            (-1, -1, -1),
            delr,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-5,
            3.0e-2,
            2,
            1.0,
            0,
        ],
        [
            5,
            (-1, -1, -1),
            delr,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-5,
            3.0e-2,
            1,
            1.0,
            0,
        ],
    ]
    connectiondata = [
        [0, -1],
        [1, 0, -2],
        [2, 1, -3],
        [3, 2, -4],
        [4, 3, -5],
        [5, 4],
    ]
    inflow = 1.0
    perioddata = {
        0: [
            [0, "inflow", inflow],
        ],
        1: [
            [3, "status", "inactive"],
            [4, "status", "inactive"],
            [5, "status", "inactive"],
        ],
    }

    sfr_dict = {
        f"{paktest}_obs.csv": [
            ("r3_out", "outflow", (2,)),
            ("r3_ext", "ext-outflow", (2,)),
            ("r6_out", "outflow", (5,)),
            ("r6_ext", "ext-outflow", (5,)),
        ]
    }
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        print_stage=True,
        print_input=True,
        print_flows=True,
        observations=sfr_dict,
        nreaches=len(packagedata),
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=perioddata,
        pname="sfr_1",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        printrecord=[("BUDGET", "LAST"), ("HEAD", "LAST")],
    )

    return sim


def check_output(idx, test):
    print("Checking sfr outflow and external outflow")
    obs_values = flopy.utils.Mf6Obs(test.workspace / f"{paktest}_obs.csv")
    test_values = {
        "R3_OUT": [-1.0, 0.0],
        "R3_EXT": [0.0, -1.0],
        "R6_OUT": [0.0, 0.0],
        "R6_EXT": [-1.0, 0.0],
    }
    for key, value in test_values.items():
        assert np.array_equal(
            obs_values.get_data(obsname=key)[key], value
        ), f"failed comparison for '{key}' observation"


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
