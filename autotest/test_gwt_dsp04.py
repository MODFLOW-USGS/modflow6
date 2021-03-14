import os
import sys
import numpy as np

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
from simulation import Simulation

# test dispersion without and with xt3d
ex = ["dsp04a", "dsp04b"]
xt3d = [None, True]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"


def get_model(idx, dir):
    nlay, nrow, ncol = 1, 21, 21
    nper = 1
    perlen = [5.0]
    nstp = [200]
    tsmult = [1.0]
    steady = [True]
    delr = 1.0
    delc = 1.0
    botm = [0.0]
    strt = 1.0
    hnoflo = 1e30
    hdry = -1e30
    hk = 1.0
    top = 1.0
    laytyp = 0
    ss = 0.0
    sy = 0.1

    chdlist = []
    ib = np.ones((nlay, nrow, ncol), dtype=int)
    ib[:, 1 : nrow - 1, 1 : ncol - 1] = 0
    idloc = np.where(ib > 0)
    for k, i, j in zip(idloc[0], idloc[1], idloc[2]):
        chdlist.append([(k, i, j), 0.0])
    chdspdict = {0: chdlist}

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

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
        model_nam_file="{}.nam".format(gwfname),
    )

    # create iterative model solution and register the gwf model with it
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwfname),
    )
    sim.register_ims_package(imsgwf, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=np.ones((nlay, nrow, ncol), dtype=int),
        filename="{}.dis".format(gwfname),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(
        gwf, strt=strt, filename="{}.ic".format(gwfname)
    )

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        save_specific_discharge=True,
        icelltype=laytyp,
        k=hk,
        k33=hk,
    )

    # chd files
    chd = flopy.mf6.ModflowGwfchd(
        gwf, stress_period_data=chdspdict, save_flows=False, pname="CHD-1"
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(gwfname),
        head_filerecord="{}.hds".format(gwfname),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
        model_nam_file="{}.nam".format(gwtname),
    )

    # create iterative model solution and register the gwt model with it
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
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
        filename="{}.ims".format(gwtname),
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
        idomain=1,
        filename="{}.dis".format(gwtname),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(
        gwt, strt=0.0, filename="{}.ic".format(gwtname)
    )

    # advection
    adv = flopy.mf6.ModflowGwtadv(
        gwt, scheme="UPSTREAM", filename="{}.adv".format(gwtname)
    )

    # advection
    xt3d_off = not xt3d[idx]
    dsp = flopy.mf6.ModflowGwtdsp(
        gwt,
        xt3d_off=xt3d_off,
        diffc=100.0,
        alh=0.0,
        alv=0.0,
        ath1=0.0,
        atv=0.0,
        filename="{}.dsp".format(gwtname),
    )

    # constant concentration
    cncs = {0: [[(0, int(nrow / 2), int(ncol / 2)), 1.0]]}
    cnc = flopy.mf6.ModflowGwtcnc(
        gwt, stress_period_data=cncs, save_flows=False, pname="CNC-1"
    )

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.1)

    # sources
    ssm = flopy.mf6.ModflowGwtssm(
        gwt, sources=[[]], filename="{}.ssm".format(gwtname)
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord="{}.cbc".format(gwtname),
        concentration_filerecord="{}.ucn".format(gwtname),
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "LAST")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename="{}.gwfgwt".format(name),
    )

    return sim


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name

    fpth = os.path.join(sim.simpath, "{}.ucn".format(gwtname))
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        conc = cobj.get_data()
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # Check to make sure that the concentrations are symmetric in both the
    # up-down and left-right directions
    concud = np.flipud(conc)
    assert np.allclose(concud, conc), (
        "simulated concentrations are not " "symmetric in up-down direction."
    )

    conclr = np.fliplr(conc)
    assert np.allclose(conclr, conc), (
        "simulated concentrations are not "
        "symmetric in left-right direction."
    )

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_transport, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
