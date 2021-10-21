"""
MODFLOW 6 Autotest
Test the interface model approach for coupling two gwf models.
We need the API for this, as the interface model is hidden and
not present in any of the output. The setup is two coupled
(3-layer) models with XT3D at the interface:

     'leftmodel'           'rightmodel'

    1  1  1  1  1         1  1  1  1  1
                    <=>
    1  1  1  1  1         1  1  1  1  1

"""
import os
import numpy as np
from modflowapi import ModflowApi
import pytest

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

ex = ["libgwf_ifmod01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# global convenience...
name_left = "leftmodel"
name_right = "rightmodel"


def build_model(idx, dir):
    name = ex[idx]

    useXT3D = True

    # parameters and spd
    # tdis
    nper = 1
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((1.0, 1, 1))

    # solver data
    nouter, ninner = 100, 300
    hclose, rclose, relax = 10 - 9, 1e-3, 0.97

    # model spatial discretization
    nlay = 3
    ncol = 5
    nrow = 2

    # cell spacing
    delr = 10.0
    delc = 10.0
    area = delr * delc

    # shift
    shift_x = 5 * delr
    shift_y = 0.0

    # top/bot of the aquifer
    tops = [0.0, -5.0, -10.0, -15.0]

    # hydraulic conductivity
    k11 = 10.0

    # boundary stress period data
    h_left = -1.0
    h_right = -2.0

    # initial head
    h_start = -2.0

    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=dir
    )

    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_hclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        inner_maximum=ninner,
        inner_hclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        relaxation_factor=relax,
    )

    # submodel on the left:
    left_chd = [
        [(ilay, irow, 0), h_left]
        for irow in range(nrow)
        for ilay in range(nlay)
    ]
    chd_spd_left = {0: left_chd}

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
        xt3doptions=useXT3D,
        save_flows=True,
        icelltype=0,
        k=k11,
    )
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_left)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord="{}.hds".format(name_left),
        budget_filerecord="{}.cbc".format(name_left),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # submodel on the right:
    right_chd = [
        [(ilay, irow, ncol - 1), h_right]
        for irow in range(nrow)
        for ilay in range(nlay)
    ]
    chd_spd_right = {0: right_chd}

    gwf = flopy.mf6.ModflowGwf(sim, modelname=name_right, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        xorigin=shift_x,
        yorigin=shift_y,
        top=tops[0],
        botm=tops[1:],
    )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        xt3doptions=useXT3D,
        save_flows=True,
        icelltype=0,
        k=k11,
    )
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_right)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord="{}.hds".format(name_right),
        budget_filerecord="{}.cbc".format(name_right),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # exchangedata
    angldegx = 0.0
    cdist = delr
    gwfgwf_data = [
        [
            (ilay, irow, ncol - 1),
            (ilay, irow, 0),
            1,
            delr / 2.0,
            delr / 2.0,
            delc,
            angldegx,
            cdist,
        ]
        for irow in range(nrow)
        for ilay in range(nlay)
    ]
    gwfgwf = flopy.mf6.ModflowGwfgwf(
        sim,
        exgtype="GWF6-GWF6",
        nexg=len(gwfgwf_data),
        exgmnamea=name_left,
        exgmnameb=name_right,
        exchangedata=gwfgwf_data,
        auxiliary=["ANGLDEGX", "CDIST"],
        xt3d=useXT3D,
    )

    return sim, None


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

    mem_addr = mf6.get_var_address("ID", name_left)
    m_id = mf6.get_value_ptr(mem_addr)[0]
    ifm_name_left = "IFM_" + str(m_id).zfill(5)
    gc_name_left = "GFC_" + str(m_id).zfill(5)

    # XT3D flag should be set to 1
    mem_addr = mf6.get_var_address("IXT3D", ifm_name_left, "NPF")
    ixt3d = mf6.get_value_ptr(mem_addr)[0]
    assert (
        ixt3d == 1
    ), "Interface model for {} should have XT3D enabled".format(name_left)

    # check if n2 > n1, then cell 1 is below 2
    mem_addr = mf6.get_var_address("TOP", ifm_name_left, "DIS")
    top = mf6.get_value_ptr(mem_addr)
    mem_addr = mf6.get_var_address("BOT", ifm_name_left, "DIS")
    bot = mf6.get_value_ptr(mem_addr)
    zc = (bot + top) / 2
    assert all(
        [zc[i] >= zc[i + 1] for i in range(len(zc) - 1)]
    ), "Interface model for {} contains incorrectly numbered cells".format(
        name_left
    )

    # confirm some properties for the 'left' interface
    # model, looping over the models that contribute:
    for name in [name_left, name_right]:
        mem_addr = mf6.get_var_address("IDXTOGLOBALIDX", gc_name_left, "GC")
        to_global = mf6.get_value_ptr(mem_addr)
        mem_addr = mf6.get_var_address("MOFFSET", name)
        m_offset = mf6.get_value_ptr(mem_addr)[0]
        mem_addr = mf6.get_var_address("NODES", name, "DIS")
        nr_nodes = mf6.get_value_ptr(mem_addr)[0]

        for i, global_id in enumerate(to_global):
            iface_idx = i + 1
            if global_id > m_offset and global_id <= m_offset + nr_nodes:
                local_id = global_id - m_offset

                # NPF/K11
                mem_addr = mf6.get_var_address("K11", name, "NPF")
                k11_model = mf6.get_value_ptr(mem_addr)
                mem_addr = mf6.get_var_address("K11", ifm_name_left, "NPF")
                k11_interface = mf6.get_value_ptr(mem_addr)
                assert (
                    k11_model[local_id - 1] == k11_interface[iface_idx - 1]
                ), "K11 in interface model does not match"

                # DIS/AREA
                mem_addr = mf6.get_var_address("AREA", name, "DIS")
                area_model = mf6.get_value_ptr(mem_addr)
                mem_addr = mf6.get_var_address("AREA", ifm_name_left, "DIS")
                area_interface = mf6.get_value_ptr(mem_addr)
                assert (
                    area_model[local_id - 1] == area_interface[iface_idx - 1]
                ), "AREA in interface model does not match"


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, exdir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, exdir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_model, idx, exdir)

    # run the test model
    test.run_mf6(Simulation(exdir, idxsim=idx, api_func=api_func))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test models
    for idx, exdir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, exdir)

        sim = Simulation(exdir, idxsim=idx, api_func=api_func)
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
