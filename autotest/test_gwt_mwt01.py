# Simple 3-layer model with a maw.  Purpose is to test pumping
# with concentration being drawn in from edge.  The aquifer
# starts with a concentration of zero, but the values grow as the boundary
# flows into the aquifer.

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

ex = ["mwt_01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))


def build_model(idx, dir):
    lx = 5.0
    lz = 3.0
    nlay = 3
    nrow = 1
    ncol = 5
    nper = 1
    delc = 1.0
    delr = lx / ncol
    delz = lz / nlay
    top = [0.0, 0.0, 0.0, 0.0, 0.0]
    botm = list(0 - np.arange(delz, nlay * delz + delz, delz))

    perlen = [0.1]
    nstp = [10]
    kstp = perlen[0] / nstp[0]
    tsmult = [1.0]

    Kh = 20.0
    Kv = 20.0

    steady = [True]
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    single_matrix = False
    nouter, ninner = 700, 300
    hclose, rclose, relax = 1e-8, 1e-6, 0.97

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwfname = "gwf_" + name

    gwf = flopy.mf6.MFModel(
        sim,
        model_type="gwf6",
        modelname=gwfname,
        model_nam_file=f"{gwfname}.nam",
    )

    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename=f"{gwfname}.ims",
    )

    idomain = np.full((nlay, nrow, ncol), 1)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=0.0)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=0,
        k=Kh,
        k33=Kv,
    )

    # chd files
    chdlist1 = [
        [(0, 0, 0), 0.0, 100.0],
        [(0, 0, ncol - 1), 0.0, 0.0],
    ]
    chd1 = flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=chdlist1,
        print_input=True,
        print_flows=True,
        save_flows=False,
        pname="CHD-1",
        auxiliary="CONCENTRATION",
        filename=f"{gwfname}.chd",
    )

    # MAW
    opth = f"{name}.maw.obs"
    wellbottom = -3.0
    wellrecarray = [[0, 0.1, wellbottom, 0.0, "THIEM", 3]]
    wellconnectionsrecarray = [
        [0, 0, (0, 0, 2), 0.0, -1, 1.0, 0.1],
        [0, 1, (1, 0, 2), -1.0, -2, 1.0, 0.1],
        [0, 2, (2, 0, 2), -2.0, -3, 1.0, 0.1],
    ]
    wellperiodrecarray = [[0, "rate", -1.0]]
    maw = flopy.mf6.ModflowGwfmaw(
        gwf,
        filename=f"{gwfname}.maw",
        print_input=True,
        print_head=True,
        print_flows=True,
        save_flows=True,
        packagedata=wellrecarray,
        connectiondata=wellconnectionsrecarray,
        perioddata=wellperiodrecarray,
        pname="MAW-1",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
        model_nam_file=f"{gwtname}.nam",
    )

    if not single_matrix:
        imsgwt = flopy.mf6.ModflowIms(
            sim,
            print_option="ALL",
            outer_dvclose=hclose,
            outer_maximum=nouter,
            under_relaxation="NONE",
            inner_maximum=ninner,
            inner_dvclose=hclose,
            rcloserecord=rclose,
            linear_acceleration="BICGSTAB",
            scaling_method="NONE",
            reordering_method="NONE",
            relaxation_factor=relax,
            filename=f"{gwtname}.ims",
        )
        sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0, filename=f"{gwtname}.ic")

    # advection
    adv = flopy.mf6.ModflowGwtadv(
        gwt, scheme="UPSTREAM", filename=f"{gwtname}.adv"
    )

    # storage
    porosity = 0.30
    sto = flopy.mf6.ModflowGwtmst(
        gwt, porosity=porosity, filename=f"{gwtname}.sto"
    )
    # sources
    sourcerecarray = [
        ("CHD-1", "AUX", "CONCENTRATION"),
        # ('WEL-1', 'AUX', 'CONCENTRATION'),
    ]
    ssm = flopy.mf6.ModflowGwtssm(
        gwt, sources=sourcerecarray, filename=f"{gwtname}.ssm"
    )

    mwt_obs = {
        (gwtname + ".mwt.obs.csv",): [
            ("mwt-1-conc", "CONCENTRATION", 1),
            ("mwt-1-rate", "RATE", 1),
        ],
    }
    # append additional obs attributes to obs dictionary
    mwt_obs["digits"] = 7
    mwt_obs["print_input"] = True
    mwt_obs["filename"] = gwtname + ".mwt.obs"

    mwtpackagedata = [
        (0, 0.0, 99.0, 999.0, "mywel"),
    ]
    mwtperioddata = [
        (0, "STATUS", "ACTIVE"),
        (0, "CONCENTRATION", 0.0),
    ]
    mwtperioddata = None

    mwt = flopy.mf6.modflow.ModflowGwtmwt(
        gwt,
        boundnames=True,
        save_flows=True,
        print_input=True,
        print_flows=True,
        print_concentration=True,
        concentration_filerecord=gwtname + ".mwt.bin",
        budget_filerecord=gwtname + ".mwt.bud",
        packagedata=mwtpackagedata,
        mwtperioddata=mwtperioddata,
        observations=mwt_obs,
        pname="MAW-1",
        auxiliary=["aux1", "aux2"],
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        concentration_filerecord=f"{gwtname}.ucn",
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL")],
        printrecord=[
            ("CONCENTRATION", "ALL"),
            ("BUDGET", "ALL"),
        ],
    )

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename=f"{name}.gwfgwt",
    )

    return sim, None


def eval_results(sim):
    print("evaluating results...")

    # ensure lake concentrations were saved
    name = ex[sim.idxsim]
    gwtname = "gwt_" + name
    fname = gwtname + ".mwt.bin"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)

    # load and check the well concentrations
    fname = gwtname + ".ucn"
    fname = os.path.join(sim.simpath, fname)
    cobj = flopy.utils.HeadFile(fname, text="CONCENTRATION")
    cmwt = cobj.get_alldata().flatten()
    print(cmwt)
    answer = np.ones(10) * 100.0
    # assert np.allclose(cmwt, answer), '{} {}'.format(cmwt, answer)

    # load the aquifer concentrations and make sure all values are correct
    fname = gwtname + ".ucn"
    fname = os.path.join(sim.simpath, fname)
    cobj = flopy.utils.HeadFile(fname, text="CONCENTRATION")
    caq = cobj.get_alldata()
    # print(caq)
    answer = np.array(
        [4.86242795, 27.24270616, 64.55536421, 27.24270616, 4.86242795]
    )
    # assert np.allclose(caq[-1].flatten(), answer), '{} {}'.format(caq[-1].flatten(), answer)

    # mwt observation results
    fpth = os.path.join(sim.simpath, gwtname + ".mwt.obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    res = tc["MWT1CONC"]
    print(res)
    answer = np.ones(10) * 100.0
    # assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc["MWT1RATE"]
    print(res)
    answer = np.ones(10) * 0.0
    # assert np.allclose(res, answer), '{} {}'.format(res, answer)

    # uncomment when testing
    # assert False

    return


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
    test.run_mf6(Simulation(dir, exfunc=eval_results, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_results, idxsim=idx)
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
