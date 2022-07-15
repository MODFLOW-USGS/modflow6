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

ex = ["ptc01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"

# read bottom data
fpth = os.path.join(ddir, "nwtp03_bot.ref")
botm = np.loadtxt(fpth, dtype=float)
nlay = 1
nrow, ncol = botm.shape
top = 200
laytyp = 1
hk = 1.0
ss = 1e-5
sy = 0.1
delr = delc = 100.0
chdloc = [(0, 49, 79), (0, 50, 79), (0, 51, 79)]
chd = 24.0
strt = botm + 20.0

# read recharge data
fpth = os.path.join(ddir, "nwtp03_rch.ref")
rch = np.loadtxt(fpth, dtype=float)


def build_mf6(idx, ws, storage=True):
    c6 = []
    for loc in chdloc:
        c6.append([loc, chd])
    cd6 = {0: c6}

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 0.01, 1.0

    nper = 1
    tdis_rc = [(1.0, 1, 1.0)]

    name = ex[idx]

    # build MODFLOW 6 files
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        model_nam_file=f"{name}.nam",
        save_flows=True,
        newtonoptions="NEWTON",
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=1e-3,
        outer_maximum=1500,
        under_relaxation="dbd",
        under_relaxation_theta=0.9,
        under_relaxation_kappa=0.0,
        under_relaxation_gamma=0.0,
        under_relaxation_momentum=0.0,
        backtracking_number=20,
        backtracking_tolerance=2.0,
        backtracking_reduction_factor=0.6,
        backtracking_residual_limit=1.0,
        inner_maximum=200,
        inner_dvclose=1e-6,
        rcloserecord="0. RELATIVE_RCLOSE",
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        preconditioner_levels=5,
        number_orthogonalizations=7,
        preconditioner_drop_tolerance=1e-4,
    )
    sim.register_ims_package(ims, [gwf.name])

    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=1,
        filename=f"{name}.dis",
    )

    # initial conditions
    flopy.mf6.ModflowGwfic(gwf, strt=strt, filename=f"{name}.ic")

    # node property flow
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=1, k=hk)

    # storage
    if storage:
        flopy.mf6.ModflowGwfsto(
            gwf, iconvert=1, ss=ss, sy=sy, steady_state={0: True}
        )

    # chd files
    flopy.mf6.modflow.ModflowGwfchd(gwf, stress_period_data=cd6)

    # rch files
    flopy.mf6.modflow.ModflowGwfrcha(gwf, recharge={0: rch})

    # output control
    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    return sim


def build_model(idx, dir):
    ws = dir
    # build mf6 with storage package but steady state stress periods
    sim = build_mf6(idx, ws, storage=True)

    # build mf6 with no storage package
    wsc = os.path.join(ws, "mf6")
    mc = build_mf6(idx, wsc, storage=False)

    return sim, mc


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for dir in exdirs:
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
