import os
import sys

import numpy as np
import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

sys.path.append("scripts")
from cross_section_functions import get_depths

paktest = "sfr"

ex = [
    "sfr_reorder",
]
exdirs = [os.path.join("temp", s) for s in ex]

# spatial discretization data
nlay, nrow, ncol = 1, 1, 1
delr, delc = 100.0, 100.0
top = 0.0
botm = -10.0
strt = 0.0

# sfr data
nreaches = 10
rlen = 10.0
rwid = 10.0
roughness = 0.001
rbth = 1.0
rhk = 0.0
slope = 0.001
ustrf = 1.0
ndv = 0

#
def build_model(idx, ws):

    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = [(11.0, 11, 1.0)]
    ts_times = np.arange(0.0, 12.0, 1.0, dtype=float)
    ts_flows = np.array([1000.0] + [float(q) for q in range(1000, -100, -100)])

    # build MODFLOW 6 files
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="seconds",
        nper=nper,
        perioddata=tdis_rc,
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        length_units="meters",
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
    npf = flopy.mf6.ModflowGwfnpf(gwf)

    # chd files
    # chd data
    spd = [
        [(0, 0, 0), 0.0],
    ]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf, stress_period_data=spd, pname="chd-1"
    )

    # sfr file
    packagedata = []
    for irch in range(nreaches):
        nconn = 1
        if 0 < irch < nreaches - 1:
            nconn += 1
        rp = [
            irch,
            "none",
            rlen,
            rwid,
            slope,
            top,
            rbth,
            rhk,
            roughness,
            nconn,
            ustrf,
            ndv,
        ]
        packagedata.append(rp)

    if not ws.endswith("mf6"):
        packagedata = packagedata[::-1]

    connectiondata = []
    if not ws.endswith("mf6"):
        inflow_loc = nreaches - 1
        ioutflow_loc = 0
        for irch in range(inflow_loc, -1, -1):
            rc = [irch]
            if irch < nreaches - 1:
                rc.append(irch + 1)
            if irch > 0:
                rc.append(-(float(irch - 1)))
            connectiondata.append(rc)
    else:
        inflow_loc = 0
        ioutflow_loc = nreaches - 1
        for irch in range(nreaches):
            rc = [irch]
            if irch > 0:
                rc.append(irch - 1)
            if irch < nreaches - 1:
                rc.append(-(irch + 1))
            connectiondata.append(rc)

    ts_names = ["inflow"]
    perioddata = [
        [inflow_loc, "inflow", "inflow"],
    ]
    ts_methods = ["linearend"] * len(ts_names)
    ts_data = []
    for t, q in zip(ts_times, ts_flows):
        ts_data.append((t, q))

    budpth = f"{name}.{paktest}.cbc"
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        print_stage=True,
        print_flows=True,
        print_input=True,
        budget_filerecord=budpth,
        mover=True,
        nreaches=nreaches,
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=perioddata,
        pname="sfr-1",
    )
    fname = f"{name}.sfr.ts"
    sfr.ts.initialize(
        filename=fname,
        timeseries=ts_data,
        time_series_namerecord=ts_names,
        interpolation_methodrecord=ts_methods,
    )
    fname = f"{name}.sfr.obs"
    sfr_obs = {
        f"{fname}.csv": [
            ("inflow", "ext-inflow", (inflow_loc,)),
            ("outflow", "ext-outflow", (ioutflow_loc,)),
        ]
    }
    sfr.obs.initialize(filename=fname, print_input=True, continuous=sfr_obs)

    # output control
    budpth = f"{name}.cbc"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=budpth,
        printrecord=[
            ("BUDGET", "ALL"),
        ],
        saverecord=[
            ("BUDGET", "ALL"),
        ],
    )

    return sim


def build_models(idx, base_ws):
    sim = build_model(idx, base_ws)

    ws = os.path.join(base_ws, "mf6")
    mc = build_model(idx, ws)

    return sim, mc


def eval_flows(sim):
    idx = sim.idxsim
    name = ex[idx]
    print("evaluating flow results..." f"({name})")

    obs_pth = os.path.join(exdirs[idx], f"{name}.sfr.obs.csv")
    obs0 = flopy.utils.Mf6Obs(obs_pth).get_data()

    obs_pth = os.path.join(exdirs[idx], "mf6", f"{name}.sfr.obs.csv")
    obs1 = flopy.utils.Mf6Obs(obs_pth).get_data()

    assert np.allclose(obs0["INFLOW"], obs1["INFLOW"]), "inflows are not equal"

    assert np.allclose(
        obs0["OUTFLOW"], obs1["OUTFLOW"]
    ), "outflows are not equal"

    fpth = os.path.join(exdirs[idx], f"{name}.lst")
    with open(fpth, "r") as f:
        lines = f.read().splitlines()

    # check order in listing file
    order = np.zeros(nreaches, dtype=int)
    for idx, line in enumerate(lines):
        if "SFR PACKAGE (SFR-1) REACH SOLUTION ORDER" in line:
            for jdx in range(nreaches):
                ipos = idx + 4 + jdx
                t = lines[ipos].split()
                order[int(t[0]) - 1] = int(t[1])
            order -= 1
            break
    actual = np.arange(nreaches, dtype=int)[::-1]

    assert np.array_equal(
        order, actual
    ), "DAG did not correctly reorder reaches."

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, exdir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, exdir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_models, idx, exdir)

    # run the test models
    test.run_mf6(
        Simulation(
            exdir,
            exfunc=eval_flows,
            idxsim=idx,
        )
    )


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test models
    for idx, exdir in enumerate(exdirs):
        test.build_mf6_models(build_models, idx, exdir)

        sim = Simulation(
            exdir,
            exfunc=eval_flows,
            idxsim=idx,
        )
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
