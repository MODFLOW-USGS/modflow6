"""
MODFLOW 6 Autotest
Test the interface model approach for coupling two DIS
models where one is translated and rotated in space:

     'model A'              'model B'
                               06     
                               07                     
                      exg      08                     
                               09
    01 02 03 04 05             10
   
 where exg couples 05 with 06 and has XT3D enabled, and model B 
 is translated and rotated
 
 The physical setup therefore is

      'model A'              'modelB'
   01 02 03 04 05   exg   06 07 08 09 10
   
 where the interface model grid (2x) is
 
   04 05 06 07

                 
 and x -->

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

ex = ["libgwf_ifmod03"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# global convenience...
name_left = "left"
name_right = "right"
global_delr = 8.0


def get_model(dir, name):

    # parameters and spd
    # tdis
    nper = 1
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((1.0, 1, 1))

    # solver data
    nouter, ninner = 100, 300
    hclose, rclose, relax = 10e-9, 1e-3, 0.97

    # model spatial discretization
    nlay = 1
    ncol = 5
    nrow = 1

    # cell spacing
    delr = global_delr
    delc = 13.0

    area = delr * delc

    # top/bot of the aquifer
    tops = [0.0, -5.0]

    # hydraulic conductivity
    k11 = 10.0

    # boundary stress period data
    h_left = -1.0
    h_right = -2.0

    # initial head
    h_start = -2.1

    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=dir,
        memory_print_option="all",
    )

    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

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

    # boundary for submodels on the left:
    left_chd = [[(0, 0, 0), h_left]]
    chd_spd_left = {0: left_chd}

    # boundary for submodel on the right:
    right_chd = [[(0, nrow - 1, 0), h_right]]
    chd_spd_right = {0: right_chd}

    # --------------------------------------
    # left model
    # --------------------------------------
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name_left, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=tops[0],
        botm=tops[1:],
    )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        save_flows=True,
        icelltype=0,
        k=k11,
    )
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_left)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{name_left}.hds",
        budget_filerecord=f"{name_left}.cbc",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # --------------------------------------
    # right model
    # --------------------------------------
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name_right, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=ncol,  # reversed!
        ncol=nrow,  # reversed!
        delr=delc,  # reversed!
        delc=delr,  # reversed!
        top=tops[0],
        botm=tops[1:],
        xorigin=2 * (5 * delr),  # note this should be twice the extent
        yorigin=0.0,
        angrot=90.0,
    )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        save_flows=True,
        icelltype=0,
        k=k11,
    )
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_right)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{name_right}.hds",
        budget_filerecord=f"{name_right}.cbc",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # exchange between left right
    angldegx = 0.0
    cdist = delr
    gwfgwf_data = [
        [
            (0, 0, ncol - 1),
            (0, 0, 0),
            1,
            delr / 2.0,
            delr / 2.0,
            delc,
            angldegx,
            cdist,
        ]
    ]
    exg1 = flopy.mf6.ModflowGwfgwf(
        sim,
        filename=name_left + "-" + name_right + ".gwfgwf",
        exgtype="GWF6-GWF6",
        nexg=len(gwfgwf_data),
        exgmnamea=name_left,
        exgmnameb=name_right,
        exchangedata=gwfgwf_data,
        auxiliary=["ANGLDEGX", "CDIST"],
        xt3d=True,
    )

    return sim


def build_model(idx, dir):
    # build MODFLOW 6 files
    ws = dir
    name = ex[idx]
    sim = get_model(ws, name)

    # build comparison model
    ws = os.path.join(dir, "libmf6")
    sim_compare = get_model(ws, name)

    return sim, sim_compare


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

    # test the interface models
    check_interface_models(mf6)

    # time loop
    current_time = mf6.get_current_time()
    end_time = mf6.get_end_time()
    while current_time < end_time:
        try:
            mf6.update()
        except:
            return api_return(success, model_ws)
        current_time = mf6.get_current_time()

    # finish
    try:
        mf6.finalize()
        success = True
    except:
        return api_return(success, model_ws)

    # cleanup and return
    return api_return(success, model_ws)


def check_interface_models(mf6):
    # interface model and connection for exchange
    exg_id = 1
    ifm = f"GWFIM1_{exg_id}"
    gfc = f"GWFCON1_{exg_id}"

    # check extent for interface model, it's always
    # DISU so we have cellxy:
    addr = mf6.get_var_address("CELLXY", ifm, "DIS")
    cellxy = mf6.get_value_ptr(addr)

    assert np.size(cellxy, 0) == 4

    xmin = np.min(cellxy[:, 0])
    ymin = np.min(cellxy[:, 1])
    xmax = np.max(cellxy[:, 0])
    ymax = np.max(cellxy[:, 1])

    # interface model extents in x direction only,
    # over 4 connected cells (it wouldn't without
    # a proper coordinate transformation)
    assert abs((xmax - xmin) - 3 * global_delr) < 1e-6
    assert abs(ymax - ymin) < 1e-6


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir, idxsim=idx, api_func=api_func))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test models
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
