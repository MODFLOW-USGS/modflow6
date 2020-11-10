"""
Test the SRC package shutoffcmin option

"""

import os
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

ex = ["src02"]
laytyp = [0]
ss = [1.0e-10]
sy = [0.1]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
nlay, nrow, ncol = 1, 1, 1


def build_models():

    perlen = [10, 10, 10]
    nper = len(perlen)
    nstp = nper * [1]
    tsmult = nper * [1.0]
    delr = 10.0
    delc = 10.0
    top = 10.0
    botm = [0.0]
    strt = top
    hk = 1.0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 0.97

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
        newtonoptions = ["NEWTON", "UNDER_RELAXATION"]
        gwf = flopy.mf6.ModflowGwf(
            sim, modelname=gwfname, newtonoptions=newtonoptions,
        )

        # create iterative model solution and register the gwf model with it
        imsgwf = flopy.mf6.ModflowIms(
            sim,
            print_option="SUMMARY",
            outer_dvclose=hclose,
            outer_maximum=nouter,
            under_relaxation="DBD",
            under_relaxation_theta=0.7,
            inner_maximum=ninner,
            inner_dvclose=hclose,
            rcloserecord=rclose,
            linear_acceleration="BICGSTAB",
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
            idomain=np.ones((nlay, nrow, ncol), dtype=np.int),
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf, save_flows=False, icelltype=laytyp[idx], k=hk, k33=hk
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
        gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname, save_flows=True)

        # create iterative model solution and register the gwt model with it
        imsgwt = flopy.mf6.ModflowIms(
            sim,
            print_option="SUMMARY",
            outer_dvclose=hclose,
            outer_maximum=nouter,
            under_relaxation="simple",
            under_relaxation_theta=1.0,
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
        ic = flopy.mf6.ModflowGwtic(gwt, strt=10.0)

        # advection
        adv = flopy.mf6.ModflowGwtadv(
            gwt, scheme="UPSTREAM", filename="{}.adv".format(gwtname)
        )

        # mass storage and transfer
        mst = flopy.mf6.ModflowGwtmst(
            gwt, porosity=sy[idx], filename="{}.mst".format(gwtname)
        )

        # src
        src_obs = {
            (gwtname + ".src.obs.csv",): [("src-1-rate", "SRC", (0, 0, 0)),],
        }
        src_obs["digits"] = 7

        # mass loading source
        srcs = {
            0: [[(0, 0, 0), -200.0, 12.6987]],
            1: [[(0, 0, 0), -200.0, 1.26987]],
            2: [[(0, 0, 0), 87.3013, 1000.0]],
        }
        src = flopy.mf6.ModflowGwtsrc(
            gwt,
            auxiliary=["constraint"],
            auxconstraintname="constraint",
            maxbound=len(srcs),
            stress_period_data=srcs,
            observations=src_obs,
            pname="SRC-1",
        )

        # output control
        oc = flopy.mf6.ModflowGwtoc(
            gwt,
            budget_filerecord="{}.cbc".format(gwtname),
            concentration_filerecord="{}.ucn".format(gwtname),
            concentrationprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
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
        conc = cobj.get_alldata().flatten()
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    fpth = os.path.join(sim.simpath, "{}.cbc".format(gwtname))
    try:
        cbbobj = flopy.utils.CellBudgetFile(fpth, precision="double")
        qsrc = []
        msrc = cbbobj.get_data(text="MASS SOURCE")
        for ra in msrc:
            qsrc.append(ra["q"][0])
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    fname = gwtname + ".src.obs.csv"
    fname = os.path.join(sim.simpath, fname)
    try:
        tc = np.genfromtxt(fname, names=True, delimiter=",")
    except:
        assert False, 'could not load data from "{}"'.format(fname)

    # calculations
    assert np.allclose(conc[0], conc[1] * 10.0)
    assert np.allclose(conc[2], 10.0)
    q_actual = np.array([26.9870, -114.2883, 87.3013])
    assert np.allclose(np.array(qsrc), q_actual)
    errmsg = "rates not equal: \n{} \n{}".format(tc["SRC1RATE"], q_actual)
    assert np.allclose(tc["SRC1RATE"], q_actual), errmsg

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
