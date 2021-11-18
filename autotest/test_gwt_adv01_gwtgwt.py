"""
MODFLOW 6 Autotest
Test the advection schemes in the gwt advection package for a one-dimensional
model grid of square cells.

"""

import os
import pytest
import sys
import numpy as np

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = ["adv01a_gwtgwt"]
scheme = ["upstream"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"


def get_gwf_model(sim, gwfname, gwfpath, modelshape, chdspd=None, welspd=None):
    nlay, nrow, ncol = modelshape
    delr = 1.0
    delc = 1.0
    top = 1.0
    botm = [0.0]
    strt = 1.0
    hk = 1.0
    laytyp = 0

    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        save_flows=True,
    )
    # this doesn't work here
    # gwf.set_model_relative_path(gwfname)

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
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=laytyp,
        k=hk,
        save_specific_discharge=True,
    )

    # chd files
    if chdspd is not None:
        chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
            gwf,
            #maxbound=len(c),
            stress_period_data=chdspd,
            save_flows=False,
            pname="CHD-1",
        )

    # wel files
    if welspd is not None:
        wel = flopy.mf6.ModflowGwfwel(
            gwf,
            print_input=True,
            print_flows=True,
            #maxbound=len(w),
            stress_period_data=welspd,
            save_flows=False,
            auxiliary="CONCENTRATION",
            pname="WEL-1",
        )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(gwfname),
        head_filerecord="{}.hds".format(gwfname),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )
    gwf.set_model_relative_path(gwfpath)
    return gwf


def get_gwt_model(sim, gwtname, gwtpath, modelshape, sourcerecarray=None):
    nlay, nrow, ncol = modelshape
    delr = 1.0
    delc = 1.0
    top = 1.0
    botm = [0.0]
    strt = 1.0
    hk = 1.0
    laytyp = 0

    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
    )
    gwt.name_file.save_flows = True

    dis = flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0)

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme="upstream")

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.1)

    # sources
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord="{}.cbc".format(gwtname),
        concentration_filerecord="{}.ucn".format(gwtname),
        concentrationprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    # obs_data = {
    #     "conc_obs.csv": [
    #         ("(1-1-10)", "CONCENTRATION", (0, 0, 9)),
    #         ("(1-1-50)", "CONCENTRATION", (0, 0, 49)),
    #     ],
    #     "flow_obs.csv": [
    #         ("c10-c11", "FLOW-JA-FACE", (0, 0, 9), (0, 0, 10)),
    #         ("c50-c51", "FLOW-JA-FACE", (0, 0, 49), (0, 0, 50)),
    #         ("c99-c100", "FLOW-JA-FACE", (0, 0, 98), (0, 0, 99)),
    #     ],
    # }
    #
    # obs_package = flopy.mf6.ModflowUtlobs(
    #     gwt,
    #     pname="conc_obs",
    #     filename="{}.obs".format(gwtname),
    #     digits=10,
    #     print_input=True,
    #     continuous=obs_data,
    # )

    gwt.set_model_relative_path(gwtpath)
    return gwt


def build_model(idx, dir):

    # temporal discretization
    nper = 1
    perlen = [5.0]
    nstp = [200]
    tsmult = [1.0]
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=ws, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units="DAYS", nper=nper,
                                 perioddata=tdis_rc, pname="sim.tdis")

    # solver settings
    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.0

    # grid information
    nlay, nrow, ncol = 1, 1, 50

    # Create gwf1 model
    welspd = {0: [[(0, 0, 0), 1.0, 1.0]]}
    chdspd = None #{0: [[(0, 0, 99), 0.0000000]]}
    gwf1 = get_gwf_model(sim, "flow1", "flow1", (nlay, nrow, ncol),
                        chdspd=chdspd, welspd=welspd)

    # Create gwf2 model
    welspd = None #{0: [[(0, 0, 0), 1.0, 1.0]]}
    chdspd = {0: [[(0, 0, ncol - 1), 0.0000000]]}
    gwf2 = get_gwf_model(sim, "flow2", "flow2", (nlay, nrow, ncol),
                        chdspd=chdspd, welspd=welspd)

    # gwf-gwf
    gwfgwf_data = [[(0, 0, ncol - 1), (0, 0, 0), 1, 0.5, 0.5, 1.0, 0.0, 1.0]]
    gwfgwf = flopy.mf6.ModflowGwfgwf(
        sim,
        exgtype="GWF6-GWF6",
        nexg=len(gwfgwf_data),
        exgmnamea=gwf1.name,
        exgmnameb=gwf2.name,
        exchangedata=gwfgwf_data,
        auxiliary=["ANGLDEGX", "CDIST"],
        filename="flow1_flow2.gwfgwf",
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
        filename="flow.ims",
    )
    sim.register_ims_package(imsgwf, [gwf1.name, gwf2.name])


    # Create gwt model
    sourcerecarray = [("WEL-1", "AUX", "CONCENTRATION")]
    gwt1 = get_gwt_model(sim, "transport1", "transport1", (nlay, nrow, ncol),
                        sourcerecarray=sourcerecarray)

    # Create gwt model
    sourcerecarray = None
    gwt2 = get_gwt_model(sim, "transport2", "transport2", (nlay, nrow, ncol),
                         sourcerecarray=sourcerecarray)

    # Create GWT GWT exchange
    gwt1gwt2 = flopy.mf6.ModflowGwtgwt(
        sim,
        exgtype="GWT6-GWT6",
        nexg=len(gwfgwf_data),
        exgmnamea=gwt1.name,
        exgmnameb=gwt2.name,
        exchangedata=gwfgwf_data,
        auxiliary=["ANGLDEGX", "CDIST"],
        filename="transport1_transport2.gwtgwt",    
    )

    # GWF GWT exchange
    gwfgwt1 = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea="flow1",
        exgmnameb="transport1",
        filename="flow1_transport1.gwfgwt",
    )
    gwfgwt2 = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea="flow2",
        exgmnameb="transport2",
        filename="flow2_transport2.gwfgwt",
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
        filename="transport.ims",
    )
    sim.register_ims_package(imsgwt, [gwt1.name, gwt2.name])
    
    # we need to use the interface model to be active for
    # the GWF part of the simulation as well (at least for now),
    # this activates it:
    os.environ["DEV_ALWAYS_USE_IFMOD"] = "1"    

    return sim, None


def eval_transport(sim):
    print("evaluating transport...")

    gwtname = "transport1"

    fpth = os.path.join(sim.simpath, gwtname, f"{gwtname}.ucn")
    try:
        cobj = flopy.utils.HeadFile(fpth, precision="double", text="CONCENTRATION")
        conc1 = cobj.get_data()
    except:
        assert False, f'could not load data from "{fpth}"'
        
    gwtname = "transport2"

    fpth = os.path.join(sim.simpath, gwtname, f"{gwtname}.ucn")
    try:
        cobj = flopy.utils.HeadFile(fpth, precision="double", text="CONCENTRATION")
        conc2 = cobj.get_data()
    except:
        assert False, f'could not load data from "{fpth}"'


    conc = np.append(conc1, conc2)
    
    # This is the answer to this problem.  These concentrations are for
    # time step 200.
    cres1 = [
        [
            [
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                1.00000000e00,
                9.99999999e-01,
                9.99999997e-01,
                9.99999991e-01,
                9.99999971e-01,
                9.99999914e-01,
                9.99999761e-01,
                9.99999372e-01,
                9.99998435e-01,
                9.99996286e-01,
                9.99991577e-01,
                9.99981712e-01,
                9.99961893e-01,
                9.99923632e-01,
                9.99852532e-01,
                9.99725120e-01,
                9.99504599e-01,
                9.99135431e-01,
                9.98536850e-01,
                9.97595635e-01,
                9.96158712e-01,
                9.94026505e-01,
                9.90948130e-01,
                9.86619748e-01,
                9.80687319e-01,
                9.72754814e-01,
                9.62398489e-01,
                9.49187176e-01,
                9.32707801e-01,
                9.12594513e-01,
                8.88559134e-01,
                8.60420154e-01,
                8.28127324e-01,
                7.91779115e-01,
                7.51630867e-01,
                7.08092322e-01,
                6.61714306e-01,
                6.13165405e-01,
                5.63200494e-01,
                5.12623768e-01,
                4.62249349e-01,
                4.12862664e-01,
                3.65185517e-01,
                3.19847250e-01,
                2.77363614e-01,
                2.38124183e-01,
                2.02388273e-01,
                1.70288648e-01,
                1.41841739e-01,
                1.16962748e-01,
                9.54838854e-02,
                7.71740354e-02,
                6.17583229e-02,
                4.89363652e-02,
                3.83983188e-02,
                2.98381826e-02,
                2.29641338e-02,
                1.75059339e-02,
                1.32196416e-02,
                9.89000005e-03,
                7.33093269e-03,
                5.38459977e-03,
                3.91944360e-03,
                2.82760119e-03,
                2.02199855e-03,
                1.43337156e-03,
                1.00739149e-03,
                7.02013580e-04,
                4.85116958e-04,
                3.32465664e-04,
                2.25991387e-04,
                1.52379541e-04,
                1.01928496e-04,
                6.76460984e-05,
                4.45462926e-05,
                2.91101871e-05,
                1.88792800e-05,
                1.21527525e-05,
                7.76522212e-06,
                4.92565188e-06,
                3.10201677e-06,
                1.93969988e-06,
                1.20440812e-06,
                7.42676511e-07,
                4.54831064e-07,
                2.76669882e-07,
                1.67174989e-07,
                1.00349240e-07,
                5.98446532e-08,
                3.54600737e-08,
            ]
        ]
    ]
    cres1 = np.array(cres1)

    creslist = [cres1]

    assert np.allclose(
        creslist[sim.idxsim], conc
    ), "simulated concentrations do not match with known solution."

    return


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
    test.run_mf6(Simulation(dir, exfunc=eval_transport, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
