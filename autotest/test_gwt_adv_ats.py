"""
Test the ATS_PERCEL option in the ADV Package for controlling the time step.
This problem is based on test_gwt_adv01, which is just 1D transport using
a well injecting with a concentration of 1 and a constant-head boundary on the
right side allowing water to leave.
"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = [
    "gwtadvats01",
]


def build_models(idx, test):
    nlay, nrow, ncol = 1, 1, 100
    nper = 1
    perlen = [5.0]
    nstp = [200]
    tsmult = [1.0]
    steady = [True]
    delr = 1.0
    delc = 1.0
    top = 1.0
    botm = [0.0]
    strt = 1.0
    hk = 1.0
    laytyp = 0

    c = {0: [[(0, 0, 99), 0.0000000]]}
    w = {0: [[(0, 0, 0), 1.0, 1.0]]}

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    name = cases[idx]

    # build MODFLOW 6 files
    ws = test.workspace
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units="DAYS", nper=nper, perioddata=tdis_rc)

    # set dt0, dtmin, dtmax, dtadj, dtfailadj
    dt0 = 0.01
    dtmin = 1.0e-5
    dtmax = perlen
    dtadj = 2.0
    dtfailadj = 5.0
    ats_filerecord = name + ".ats"
    atsperiod = [(0, dt0, dtmin, dtmax[i], dtadj, dtfailadj) for i in range(nper)]
    tdis.ats.initialize(
        maxats=len(atsperiod),
        perioddata=atsperiod,
        filename=ats_filerecord,
    )

    # create gwf model
    gwfname = "gwf_" + name
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        save_flows=True,
        model_nam_file=f"{gwfname}.nam",
    )

    # create iterative model solution and register the gwf model with it
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        ats_outer_maximum_fraction=0.0,
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
        filename=f"{gwfname}.ims",
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
        filename=f"{gwfname}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename=f"{gwfname}.ic")

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        icelltype=laytyp,
        k=hk,
        k33=hk,
        save_specific_discharge=True,
    )

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
        gwf,
        maxbound=len(c),
        stress_period_data=c,
        save_flows=False,
        pname="CHD-1",
    )

    # wel files
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        print_input=True,
        print_flows=True,
        maxbound=len(w),
        stress_period_data=w,
        save_flows=False,
        auxiliary="CONCENTRATION",
        pname="WEL-1",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
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
    gwt.name_file.save_flows = True

    # create iterative model solution and register the gwt model with it
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        ats_outer_maximum_fraction=0.0,
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
        idomain=1,
        filename=f"{gwtname}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0, filename=f"{gwtname}.ic")

    # advection
    adv = flopy.mf6.ModflowGwtadv(
        gwt, scheme="upstream", ats_percel=0.5, filename=f"{gwtname}.adv"
    )

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.1)

    # sources
    sourcerecarray = [("WEL-1", "AUX", "CONCENTRATION")]
    ssm = flopy.mf6.ModflowGwtssm(
        gwt, sources=sourcerecarray, filename=f"{gwtname}.ssm"
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        concentration_filerecord=f"{gwtname}.ucn",
        concentrationprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "LAST")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    obs_data = {
        "conc_obs.csv": [
            ("(1-1-10)", "CONCENTRATION", (0, 0, 9)),
            ("(1-1-50)", "CONCENTRATION", (0, 0, 49)),
        ],
        "flow_obs.csv": [
            ("c10-c11", "FLOW-JA-FACE", (0, 0, 9), (0, 0, 10)),
            ("c50-c51", "FLOW-JA-FACE", (0, 0, 49), (0, 0, 50)),
            ("c99-c100", "FLOW-JA-FACE", (0, 0, 98), (0, 0, 99)),
        ],
    }

    obs_package = flopy.mf6.ModflowUtlobs(
        gwt,
        pname="conc_obs",
        filename=f"{gwtname}.obs",
        digits=10,
        print_input=True,
        continuous=obs_data,
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


def check_output(idx, test):
    name = cases[idx]
    gwtname = "gwt_" + name

    fpth = os.path.join(test.workspace, f"{gwtname}.ucn")
    try:
        cobj = flopy.utils.HeadFile(fpth, precision="double", text="CONCENTRATION")
        conc = cobj.get_data()
        times = cobj.times
    except:
        assert False, f'could not load data from "{fpth}"'

    # Verify number of time steps; there should be 101 of them.
    assert len(times) == 101, f"Found {len(times)} time steps.  Expecting 102."

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
    diff = cres1 - conc
    print(f"{diff.min()=} {diff.max()=}")
    assert np.allclose(cres1, conc, atol=0.03), (
        "simulated concentrations do not match with known solution: "
        f"{diff.min()=} {diff.max()=}"
    )


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
