"""

Same as test_swf_dfw_swr2.py except this one uses
the adaptive time stepping (ATS).  

"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = [
    "swf-swr-t2b",
]


def build_models(idx, test):
    dx = 500.0
    nreach = 11
    nper = 1
    perlen = [5040 * 2 * 60.0]  # 7 days (in seconds)
    nstp = [50]  # In SWR report nstp = [5040] and tsmult is 1.
    tsmult = [1.2]

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    name = "swf"

    # build MODFLOW 6 files
    ws = test.workspace
    sim = flopy.mf6.MFSimulation(
        sim_name=f"{name}_sim", version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="SECONDS", nper=nper, perioddata=tdis_rc
    )

    # set dt0, dtmin, dtmax, dtadj, dtfailadj
    dt0 = 60 * 60.0 * 24.0  # 24 hours
    dtmin = 1.0 * 60.0  # 1 minute
    dtmax = 60 * 60.0 * 24.0  # 24 hours
    dtadj = 2.0
    dtfailadj = 5.0
    ats_filerecord = name + ".ats"
    atsperiod = [
        (0, dt0, dtmin, dtmax, dtadj, dtfailadj),
    ]
    tdis.ats.initialize(
        maxats=len(atsperiod),
        perioddata=atsperiod,
        filename=ats_filerecord,
    )

    # surface water model
    swfname = f"{name}_model"
    swf = flopy.mf6.ModflowSwf(sim, modelname=swfname, save_flows=True)

    nouter, ninner = 10, 50
    hclose, rclose, relax = 1e-8, 1e-8, 1.0
    imsswf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        under_relaxation_theta=0.9,
        under_relaxation_kappa=0.0001,
        under_relaxation_gamma=0.0,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        # backtracking_number=5,
        # backtracking_tolerance=1.0,
        # backtracking_reduction_factor=0.3,
        # backtracking_residual_limit=100.0,
        filename=f"{swfname}.ims",
    )
    sim.register_ims_package(imsswf, [swf.name])

    vertices = []
    vertices = [[j, j * dx, 0.0, 0.0] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    nodes = len(cell2d)
    nvert = len(vertices)

    reach_bottom = np.linspace(1.05, 0.05, nreach)

    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        nodes=nodes,
        nvert=nvert,
        reach_length=dx,
        reach_bottom=reach_bottom,
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    dfw = flopy.mf6.ModflowSwfdfw(
        swf,
        print_flows=True,
        save_flows=True,
        width=dx,
        manningsn=0.30,
        slope=0.05 / 500.0,
        idcxs=None,
    )

    sto = flopy.mf6.ModflowSwfsto(
        swf,
        save_flows=True,
    )

    ic = flopy.mf6.ModflowSwfic(
        swf,
        strt=2.05,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{swfname}.bud",
        stage_filerecord=f"{swfname}.stage",
        saverecord=[
            ("STAGE", "ALL"),
            ("BUDGET", "ALL"),
        ],
        printrecord=[
            ("STAGE", "ALL"),
            ("BUDGET", "ALL"),
        ],
    )

    # flw
    inflow_reach = 0
    qinflow = 23.570
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(inflow_reach, qinflow)],
    )

    chd = flopy.mf6.ModflowSwfchd(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(nreach - 1, 1.05)],
    )

    obs_data = {
        f"{swfname}.obs.csv": [
            ("OBS1", "STAGE", (1,)),
            ("OBS2", "STAGE", (5,)),
            ("OBS3", "STAGE", (8,)),
        ],
    }
    obs_package = flopy.mf6.ModflowUtlobs(
        swf,
        filename=f"{swfname}.obs",
        digits=10,
        print_input=True,
        continuous=obs_data,
    )

    return sim, None


def make_plot(test, mfsim):
    print("making plots...")
    import matplotlib.pyplot as plt

    fpth = test.workspace / f"swf_model.obs.csv"
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(1, 1, 1)
    for irch in [1, 2, 3]:
        ax.plot(
            obsvals["time"] / 3600.0,
            obsvals[f"OBS{irch}"],
            marker="o",
            mfc="none",
            mec="k",
            lw=0.0,
            label=f"MF6 reach {irch}",
        )
        # ax.plot(obsvals["time"], answer[f"STAGE00000000{irch:02d}"], "k-", label=f"SWR Reach {irch}")
    ax.set_xlim(0, 30.0)
    ax.set_ylim(1.2, 2.4)
    plt.xlabel("time, in hours")
    plt.ylabel("stage, in meters")
    plt.legend()
    fname = test.workspace / "swf_model.obs.1.png"
    plt.savefig(fname)

    return


def check_output(idx, test):
    print(f"evaluating model for case {idx}...")

    swfname = "swf_model"
    ws = test.workspace
    mfsim = flopy.mf6.MFSimulation.load(sim_ws=ws)

    makeplot = False
    if makeplot:
        make_plot(test, mfsim)

    # read binary stage file
    fpth = test.workspace / f"{swfname}.stage"
    sobj = flopy.utils.HeadFile(fpth, precision="double", text="STAGE")
    stage_all = sobj.get_alldata()
    # for kstp, stage in enumerate(stage_all):
    #     print(kstp, stage.flatten())

    # at end of simulation, water depth should be 1.0 for all reaches
    swf = mfsim.get_model(swfname)
    depth = stage_all[-1] - swf.disl.reach_bottom.array
    np.allclose(
        depth, 1.0
    ), f"Simulated depth at end should be 1, but found {depth}"


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
    )
    test.run()
