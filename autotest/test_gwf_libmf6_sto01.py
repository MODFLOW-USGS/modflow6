"""
MODFLOW 6 Autotest
Test the bmi set_value function, which is used update
the Sy=0 value with same Sy used to calculate SC2 in
the non-bmi simulation.
"""

import os

import numpy as np
import pytest
from modflowapi import ModflowApi

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation, api_return

ex = ["libgwf_sto01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# average recharge rate
avg_rch = 0.001

# calculate recharge rates
dx = 1 / 20
rad = np.arange(0, 1 + dx, dx) * 2.0 * np.pi
f = np.sin(rad)
rch_rates = avg_rch + f * avg_rch

# temporal discretization
nper = rch_rates.shape[0]
tdis_rc = []
for i in range(nper):
    tdis_rc.append((1.0, 1, 1))

# model spatial dimensions
nlay, nrow, ncol = 1, 1, 100

# cell spacing
delr = 50.0
delc = 1.0
area = delr * delc

# top of the aquifer
top = 25.0

# bottom of the aquifer
botm = 0.0

# hydraulic conductivity
hk = 50.0

# boundary heads
h1 = 20.0
h2 = 11.0

# build chd stress period data
chd_spd = {0: [[(0, 0, 0), h1], [(0, 0, ncol - 1), h2]]}

strt = np.linspace(h1, h2, num=ncol)

# build recharge spd
rch_spd = {}
for n in range(nper):
    rch_spd[n] = rch_rates[n]

# storage variables
sy_val = 0.2

# solver data
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97


def get_model(ws, name, sy):
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        memory_print_option="all",
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        relaxation_factor=relax,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)

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
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True, icelltype=1, k=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf, save_flows=True, iconvert=1, ss=0.0, sy=sy, transient={0: True}
    )

    # chd file
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)

    # recharge file
    rch = flopy.mf6.ModflowGwfrcha(gwf, recharge=rch_spd)

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )
    return sim


def build_model(idx, dir):
    # build MODFLOW 6 files
    ws = dir
    name = ex[idx]
    sim = get_model(ws, name, sy=sy_val)

    # build comparison model
    ws = os.path.join(dir, "libmf6")
    mc = get_model(ws, name, sy=0.0)

    return sim, mc


def api_func(exe, idx, model_ws=None):
    success = False

    name = ex[idx].upper()
    if model_ws is None:
        model_ws = "."

    try:
        mf6 = ModflowApi(exe, working_directory=model_ws)
    except Exception as e:
        print("Failed to load " + exe)
        print("with message: " + str(e))
        return api_return(success, model_ws)

    # initialize the model
    try:
        mf6.initialize()
    except:
        return api_return(success, model_ws)

    # time loop
    current_time = mf6.get_current_time()
    end_time = mf6.get_end_time()

    # reset sy with bmi set_value
    sy_tag = mf6.get_var_address("SY", name, "STO")
    new_sy = mf6.get_value(sy_tag)
    new_sy.fill(sy_val)

    mf6.set_value(sy_tag, new_sy)

    # model time loop
    idx = 0
    while current_time < end_time:

        # run the time step
        try:
            mf6.update()
        except:
            return api_return(success, model_ws)

        # update time
        current_time = mf6.get_current_time()

        # increment counter
        idx += 1

    # cleanup
    try:
        mf6.finalize()
        success = True
    except:
        return api_return(success, model_ws)

    # cleanup and return
    return api_return(success, model_ws)


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
    test.run_mf6(Simulation(dir, idxsim=idx, api_func=api_func))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, idxsim=idx, api_func=api_func)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
