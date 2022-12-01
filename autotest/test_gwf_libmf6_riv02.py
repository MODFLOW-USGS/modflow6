"""
MODFLOW 6 Autotest
Test the api which is used set hcof and rhs in api package compare to river
package in the non-api simulation.
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

ex = ["libgwf_riv02"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))


# temporal discretization
nper = 10
tdis_rc = []
for i in range(nper):
    tdis_rc.append((1.0, 1, 1))

# model spatial dimensions
nlay, nrow, ncol = 1, 1, 10

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
h1 = 11.0
h2 = 11.0

# build chd stress period data
chd_spd = {0: [[(0, 0, 0), h1], [(0, 0, ncol - 1), h2]]}

strt = np.linspace(h1, h2, num=ncol)


# solver data
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97

# uniform river stage
riv_stage = 15.0
riv_stage2 = 20.0
riv_bot = 12.0
riv_cond = 35.0
riv_packname = "MYRIV"


def get_model(ws, name, riv_spd, api=False):
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
        gwf, save_flows=True, iconvert=1, ss=0.0, sy=0.2, transient={0: True}
    )

    # chd file
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)

    # riv package
    if api:
        flopy.mf6.ModflowGwfapi(gwf, maxbound=ncol - 2, pname=riv_packname)
    else:
        riv = flopy.mf6.ModflowGwfriv(
            gwf, stress_period_data=riv_spd, pname=riv_packname
        )

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

    # create river data
    rd = [
        [(0, 0, icol), riv_stage, riv_cond, riv_bot]
        for icol in range(1, ncol - 1)
    ]
    rd2 = [
        [(0, 0, icol), riv_stage2, riv_cond, riv_bot]
        for icol in range(1, ncol - 1)
    ]
    sim = get_model(ws, name, riv_spd={0: rd, 5: rd2})

    # build comparison model with zeroed values
    ws = os.path.join(dir, "libmf6")
    rd_api = [[(0, 0, icol), 999.0, 999.0, 0.0] for icol in range(1, ncol - 1)]
    mc = get_model(ws, name, riv_spd={0: rd_api}, api=True)

    return sim, mc


def api_riv_pak(stage, h, hcof, rhs):
    for idx, icol in enumerate(range(1, ncol - 1)):
        if h[icol] > riv_bot:
            hcof[idx] = -riv_cond
            rhs[idx] = -riv_cond * stage
        else:
            hcof[idx] = 0.0
            rhs[idx] = -riv_cond * (stage - riv_bot)
    return hcof, rhs


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

    # maximum outer iterations
    max_iter = mf6.get_value(mf6.get_var_address("MXITER", "SLN_1"))

    # get pointer to simulated heads
    head_tag = mf6.get_var_address("X", name.upper())
    head = mf6.get_value_ptr(head_tag)

    # get pointers to API data
    nbound_tag = mf6.get_var_address("NBOUND", name.upper(), riv_packname)
    nbound = mf6.get_value_ptr(nbound_tag)
    nodelist_tag = mf6.get_var_address("NODELIST", name.upper(), riv_packname)
    nodelist = mf6.get_value_ptr(nodelist_tag)
    hcof_tag = mf6.get_var_address("HCOF", name.upper(), riv_packname)
    hcof = mf6.get_value_ptr(hcof_tag)
    rhs_tag = mf6.get_var_address("RHS", name.upper(), riv_packname)
    rhs = mf6.get_value_ptr(rhs_tag)

    # set nbound and nodelist
    nbound[0] = ncol - 2
    for idx, icol in enumerate(range(1, ncol - 1)):
        nodelist[idx] = icol + 1

    # model time loop
    idx = 0
    while current_time < end_time:

        # get dt
        dt = mf6.get_time_step()

        # prepare... and reads the RIV data from file!
        mf6.prepare_time_step(dt)

        if current_time < 5:
            stage = riv_stage
        else:
            stage = riv_stage2

        # convergence loop
        kiter = 0
        mf6.prepare_solve()

        while kiter < max_iter:
            # update api package
            hcof[:], rhs[:] = api_riv_pak(
                stage,
                head,
                hcof,
                rhs,
            )

            # solve with updated api data
            has_converged = mf6.solve()
            kiter += 1

            if has_converged:
                msg = f"Converged in {kiter}" + " outer iterations"
                print(msg)
                break

        # finalize time step
        mf6.finalize_solve()

        # finalize time step and update time
        mf6.finalize_time_step()
        current_time = mf6.get_current_time()

        # terminate if model did not converge
        if not has_converged:
            print("model did not converge")
            break

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
