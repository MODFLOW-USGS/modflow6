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

ex = ["mst01"]
laytyp = [1]
ss = [0.0]
sy = [0.1]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
nlay, nrow, ncol = 4, 1, 1


def build_models():

    nper = 1
    perlen = [3.0]
    nstp = [3]
    tsmult = [1.0]
    delr = 1.0
    delc = 1.0
    top = 4.0
    botm = [3.0, 2.0, 1.0, 0.0]
    hnoflo = 1e30
    hdry = -1e30
    strt = [hdry, hdry, hdry, 0.5]
    hk = 1.0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.0

    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

    for idx, dir in enumerate(exdirs):
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
            icelltype=laytyp[idx],
            k=hk,
            k33=hk,
            rewet_record=[("WETFCT", 0.01, "IWETIT", 1, "IHDWET", 0)],
            wetdry=0.01,
        )
        # storage
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=False,
            iconvert=laytyp[idx],
            ss=ss[idx],
            sy=sy[idx],
            steady_state={0: False},
            transient={0: True},
        )

        # wel files
        welspdict = {0: [[(3, 0, 0), 0.1, 1.0]], 1: [[(3, 0, 0), -0.1, 0.0]]}
        wel = flopy.mf6.ModflowGwfwel(
            gwf,
            print_input=True,
            print_flows=True,
            stress_period_data=welspdict,
            save_flows=False,
            auxiliary="CONCENTRATION",
            pname="WEL-1",
        )

        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord="{}.cbc".format(gwfname),
            head_filerecord="{}.hds".format(gwfname),
            headprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("HEAD", "ALL")],
            printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
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
            gwt, strt=1.0, filename="{}.ic".format(gwtname)
        )

        # advection
        adv = flopy.mf6.ModflowGwtadv(
            gwt, scheme="UPSTREAM", filename="{}.adv".format(gwtname)
        )

        # mass storage and transfer
        mst = flopy.mf6.ModflowGwtmst(
            gwt, porosity=sy[idx], filename="{}.mst".format(gwtname)
        )

        # sources
        sourcerecarray = [("WEL-1", "AUX", "CONCENTRATION")]
        ssm = flopy.mf6.ModflowGwtssm(
            gwt, sources=sourcerecarray, filename="{}.ssm".format(gwtname)
        )

        # output control
        oc = flopy.mf6.ModflowGwtoc(
            gwt,
            budget_filerecord="{}.cbc".format(gwtname),
            concentration_filerecord="{}.ucn".format(gwtname),
            concentrationprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("CONCENTRATION", "ALL")],
            printrecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        )

        # GWF GWT exchange
        gwfgwt = flopy.mf6.ModflowGwfgwt(
            sim,
            exgtype="GWF6-GWT6",
            exgmnamea=gwfname,
            exgmnameb=gwtname,
            filename="{}.gwfgwt".format(name),
        )

        # write MODFLOW 6 files
        sim.write_simulation()

    return


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name

    fpth = os.path.join(sim.simpath, "{}.ucn".format(gwtname))
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        conc1 = cobj.get_data(totim=3.0)
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # end of stress period 1
    cres1 = np.ones((nlay, nrow, ncol), float)
    assert np.allclose(cres1, conc1), (
        "simulated concentrations do not match " "with known solution."
    )

    return


# - No need to change any code below
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
