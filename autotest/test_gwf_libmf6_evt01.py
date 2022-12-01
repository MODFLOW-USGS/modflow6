"""
MODFLOW 6 Autotest
Test the bmi which is used update the calculate a head-based pumping rate that
is equivalent to use of the evapotranspiration package in the
non-bmi simulation.
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

ex = ["libgwf_evt01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# et variables
et_max = 0.1
et_depth = 5.0

# temporal discretization
nper = 100
tdis_rc = []
for i in range(nper):
    tdis_rc.append((1.0, 1, 1))

# model spatial dimensions
nlay, nrow, ncol = 1, 1, 1

# cell spacing
delr = 10.0
delc = 10.0
area = delr * delc

# top of the aquifer
top = 10.0

# bottom of the aquifer
botm = 0.0

# hydraulic conductivity
hk = 50.0

# starting head
strt = 15.0

# solver data
nouter, ninner = 100, 100
hclose, rclose, relax = 1e-9, 1e-3, 0.97


def get_model(ws, name, bmi=False):
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
        under_relaxation="SIMPLE",
        under_relaxation_gamma=0.98,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        relaxation_factor=relax,
    )

    # create gwf model
    newtonoptions = "NEWTON UNDER_RELAXATION"
    gwf = flopy.mf6.ModflowGwf(
        sim,
        newtonoptions=newtonoptions,
        modelname=name,
        print_input=True,
        save_flows=True,
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
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True, icelltype=1, k=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf, save_flows=True, iconvert=1, ss=1e-5, sy=0.2, transient={0: True}
    )

    # evapotranspiration
    if not bmi:
        evt = flopy.mf6.ModflowGwfevta(
            gwf, surface=top, rate=et_max, depth=et_depth
        )
    wel = flopy.mf6.ModflowGwfwel(gwf, stress_period_data=[[(0, 0, 0), 0.0]])

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
    sim = get_model(ws, name)

    # build comparison model
    ws = os.path.join(dir, "libmf6")
    mc = get_model(ws, name, bmi=True)

    return sim, mc


def head2et_wellrate(h):
    if h > top:
        q = -et_max
    elif h < top - et_depth:
        q = 0.0
    else:
        f = (h - (top - et_depth)) / et_depth
        q = -f * et_max
    return q * area


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

    # get pointer to simulated heads
    head_tag = mf6.get_var_address("X", "LIBGWF_EVT01")
    head = mf6.get_value_ptr(head_tag)

    # maximum outer iterations
    mxit_tag = mf6.get_var_address("MXITER", "SLN_1")
    max_iter = mf6.get_value(mxit_tag)

    # get copy of well data
    well_tag = mf6.get_var_address("BOUND", name, "WEL_0")
    well = mf6.get_value(well_tag)

    # check NPF type
    package_type_tag = mf6.get_var_address("PACKAGE_TYPE", name, "NPF")
    package_type = mf6.get_value(package_type_tag)[0]
    assert package_type == "NPF", f"{package_type} /= NPF"

    # check wel type
    package_type_tag = mf6.get_var_address("PACKAGE_TYPE", name, "WEL_0")
    package_type = mf6.get_value(package_type_tag)[0]
    assert package_type == "WEL", f"{package_type} /= WEL"

    twell = np.zeros(ncol, dtype=np.float64)

    # model time loop
    idx = 0
    while current_time < end_time:

        # get dt and prepare for non-linear iterations
        dt = mf6.get_time_step()
        mf6.prepare_time_step(dt)

        # convergence loop
        kiter = 0
        mf6.prepare_solve()

        while kiter < max_iter:

            # update well rate
            twell[:] = head2et_wellrate(head[0])
            well[:, 0] = twell[:]
            mf6.set_value(well_tag, well)

            # solve with updated well rate
            has_converged = mf6.solve()
            kiter += 1

            if has_converged:
                msg = (
                    f"Component {1}"
                    + f" converged in {kiter}"
                    + " outer iterations"
                )
                print(msg)
                break

        if not has_converged:
            return api_return(success, model_ws)

        # finalize time step
        mf6.finalize_solve()

        # finalize time step and update time
        mf6.finalize_time_step()
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
