"""
# Test the ability of a uzf to route waves through a simple 1d vertical
# column.

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

ex = ["gwf_uzf01a"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
nlay, nrow, ncol = 100, 1, 1

def build_models():

    perlen = [500.]
    nper = len(perlen)
    nstp = [10]
    tsmult = nper * [1.0]
    delr = 1.0
    delc = 1.0
    delv = 1.0
    top = 100.0
    botm = [top - (k + 1) * delv for k in range(nlay)]
    strt = 0.5
    hk = 1.0
    laytyp = 1
    ss = 0.
    sy = 0.1

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
        tdis = flopy.mf6.ModflowTdis(sim,
                                     time_units='DAYS',
                                     nper=nper,
                                     perioddata=tdis_rc)

        # create gwf model
        gwfname = name
        newtonoptions = ["NEWTON", "UNDER_RELAXATION"]
        gwf = flopy.mf6.ModflowGwf(
            sim,
            modelname=gwfname,
            newtonoptions=newtonoptions,
        )

        # create iterative model solution and register the gwf model with it
        nouter, ninner = 20, 10
        hclose, rclose, relax = 1.5e-6, 1e-6, 0.97
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
            idomain=np.ones((nlay, nrow, ncol), dtype=int),
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf, save_flows=False, icelltype=laytyp, k=hk
        )
        # storage
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=False,
            iconvert=laytyp,
            ss=ss,
            sy=sy,
            steady_state={0: False},
            transient={0: True},
        )

        # ghb
        ghbspdict = {
            0: [[(nlay - 1, 0, 0), 1.5, 1.0]],
        }
        ghb = flopy.mf6.ModflowGwfghb(
            gwf,
            print_input=True,
            print_flows=True,
            stress_period_data=ghbspdict,
            save_flows=False,
        )

        sd = 0.1
        vks = hk
        thtr = 0.05
        thti = thtr
        thts = sy
        eps = 4
        uzf_pkdat = (
            [[0, (0, 0, 0), 1, 1, sd, vks, thtr, thts, thti, eps, "uzf01"]] +
            [[k, (k, 0, 0), 0, k + 1, sd, vks, thtr, thts, thti, eps, "uzf0{}".format(k+1)]
            for k in range(1, nlay - 1)]
        )
        uzf_pkdat[-1][3] = -1
        infiltration = 0.01
        uzf_spd = {0: [[0, infiltration, 0., 0., 0., 0., 0., 0.],]}
        uzf = flopy.mf6.ModflowGwfuzf(
            gwf,
            print_input=True,
            print_flows=True,
            save_flows=True,
            boundnames=True,
            ntrailwaves=15,
            nwavesets=40,
            nuzfcells=len(uzf_pkdat),
            packagedata=uzf_pkdat,
            perioddata=uzf_spd,
            budget_filerecord="{}.uzf.bud".format(name),
            filename="{}.uzf".format(name),
        )

        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord="{}.bud".format(gwfname),
            head_filerecord="{}.hds".format(gwfname),
            headprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
            printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        )

        obs_lst = []
        obs_lst.append(['obs1', "head", (0, 0, 0)])
        obs_lst.append(['obs2', "head", (1, 0, 0)])
        obs_dict = {"{}.obs.csv".format(gwfname): obs_lst}
        obs = flopy.mf6.ModflowUtlobs(
            gwf, pname="head_obs", digits=20, continuous=obs_dict
        )

        # write MODFLOW 6 files
        sim.write_simulation()

    return


def eval_flow(sim):
    print("evaluating flow...")

    name = ex[sim.idxsim]
    ws = exdirs[sim.idxsim]

    bpth = os.path.join(ws, name + ".uzf.bud")
    bobj = flopy.utils.CellBudgetFile(bpth, precision="double")
    print(bobj.get_unique_record_names())
    gwf_recharge = bobj.get_data(text='GWF')

    bpth = os.path.join(ws, name + ".bud")
    bobj = flopy.utils.CellBudgetFile(bpth, precision="double")
    #print(bobj.get_unique_record_names())
    uzf_recharge = bobj.get_data(text='UZF-GWRCH')
    errmsg = "uzf rch is not equal to negative gwf rch"
    for gwr, uzr in zip(gwf_recharge, uzf_recharge):
        print(uzr["q"].min(), uzr["q"].max())
        print(gwr["q"].min(), gwr["q"].max())
        assert np.allclose(gwr["q"], -uzr["q"]), errmsg

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_flow, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_flow, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
