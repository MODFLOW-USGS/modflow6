"""

Loop network problem from SWR Manual (Problem 4)

"""

import os

import flopy
import numpy as np
import pytest

from conftest import project_root_path
from framework import TestFramework

cases = [
    "swf-dfw-loop",
]

data_path = project_root_path / "autotest/data/swr04/"
fpth = data_path / "SWRSample04_Stage.csv.cmp"
answer = np.genfromtxt(fpth, names=True, delimiter=",")
print(answer)

fpth = data_path / "SWRFlows.csv.cmp"
answer_flow = np.genfromtxt(fpth, names=True, delimiter=",")
print(answer_flow)


def build_models(idx, test):

    sim_ws = test.workspace
    name = cases[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=sim_ws,
        memory_print_option="summary",
    )

    tdis = flopy.mf6.ModflowTdis(
        sim, nper=1, perioddata=[(604800.0, 336, 1.0)], time_units="SECONDS"
    )
    ims = flopy.mf6.ModflowIms(
        sim,
        outer_maximum=100,
        inner_maximum=50,
        print_option="all",
        outer_dvclose=1.0e-6,
        inner_dvclose=1.0e-6,
        linear_acceleration="BICGSTAB",
        backtracking_number=5,
        backtracking_tolerance=1.0,
        backtracking_reduction_factor=0.3,
        backtracking_residual_limit=100.0,
    )
    swf = flopy.mf6.ModflowSwf(
        sim,
        modelname=name,
        save_flows=True,
    )

    vertices = [
        [0, 500.0, 6000.0, 0.0],
        [1, 500.0, 5000.0, 0.0],
        [2, 500.0, 4500.0, 0.0],
        [3, 1000.0, 4500.0, 0.0],
        [4, 2000.0, 4500.0, 0.0],
        [5, 3000.0, 4500.0, 0.0],
        [6, 3500.0, 4500.0, 0.0],
        [7, 3500.0, 4000.0, 0.0],
        [8, 3500.0, 3000.0, 0.0],
        [9, 3500.0, 2500.0, 0.0],
        [10, 3500.0, 2000.0, 0.0],
        [11, 3500.0, 1000.0, 0.0],
        [12, 3500.0, 500.0, 0.0],
        [13, 4000.0, 500.0, 0.0],
        [14, 5000.0, 500.0, 0.0],
        [15, 6000.0, 500.0, 0.0],
        [16, 500.0, 4000.0, 0.0],
        [17, 1000.0, 3000.0, 0.0],
        [18, 2000.0, 2500.0, 0.0],
        [19, 3000.0, 2500.0, 0.0],
    ]

    cell2d = [
        [0, 0.5, 2, 0, 1],
        [1, 0.5, 2, 1, 2],
        [2, 0.5, 2, 2, 3],
        [3, 0.5, 2, 3, 4],
        [4, 0.5, 2, 4, 5],
        [5, 0.5, 3, 5, 6, 7],
        [6, 0.5, 2, 7, 8],
        [7, 0.5, 2, 8, 9],
        [8, 0.5, 2, 2, 16],
        [9, 0.5, 2, 16, 17],
        [10, 0.5, 2, 17, 18],
        [11, 0.5, 2, 18, 19],
        [12, 0.5, 2, 19, 9],
        [13, 0.5, 2, 9, 10],
        [14, 0.5, 2, 10, 11],
        [15, 0.5, 3, 11, 12, 13],
        [16, 0.5, 2, 13, 14],
        [17, 0.5, 2, 14, 15],
    ]

    toreach = [1, 2, 3, 4, 5, 6, 7, 13, 9, 10, 11, 12, 13, 14, 15, 16, 17, -1]

    reach_length = [
        1000.0,
        500.0,
        500.0,
        1000.0,
        1000.0,
        1000.0,
        1000.0,
        500.0,
        500.0,
        1118.0,
        1118.0,
        1000.0,
        500.0,
        500.0,
        1000.0,
        1000.0,
        1000.0,
        1000.0,
    ]

    width = [
        10.0,
        10.0,
        20.0,
        20.0,
        20.0,
        20.0,
        20.0,
        20.0,
        10.0,
        10.0,
        10.0,
        10.0,
        10.0,
        15.0,
        15.0,
        15.0,
        15.0,
        15.0,
    ]

    reach_bottom = [
        2.0,
        1.83,
        1.83,
        1.67,
        1.50,
        1.33,
        1.17,
        1.0,
        1.83,
        1.65,
        1.41,
        1.17,
        1.0,
        1.0,
        0.75,
        0.50,
        0.25,
        0.0,
    ]

    idcxs = [
        0,
        0,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        2,
        2,
        2,
        2,
        2,
    ]

    # time, reach1, reach5 (cms)
    reach_inflow = [
        (0, 0, 0),
        (3600, 0, 0),
        (7200, 0, 0),
        (10800, 0.002, 0),
        (14400, 0.01, 0),
        (18000, 0.05, 0),
        (21600, 0.2, 0),
        (25200, 0.671, 0),
        (28800, 1.868, 0),
        (32400, 4.315, 0),
        (36000, 8.277, 0),
        (39600, 13.183, 0),
        (43200, 17.4232, 0),
        (46800, 19.1223, 0.002),
        (50400, 17.423, 0.079),
        (54000, 13.18, 1.199),
        (57600, 8.277, 8.313),
        (61200, 4.315, 26.486),
        (64800, 1.868, 38.952),
        (68400, 0.671, 26.486),
        (72000, 0.2, 8.313),
        (75600, 0.05, 1.199),
        (79200, 0.01, 0.079),
        (82800, 0.002, 0.002),
        (86400, 0, 0),
        (604800, 0, 0),
        (608400, 0, 0),
    ]

    nodes = len(cell2d)
    nvert = len(vertices)

    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        nodes=nodes,
        nvert=nvert,
        reach_length=reach_length,
        reach_bottom=reach_bottom,
        # toreach=toreach,   # -1 gives 0 in one-based, which means outflow cell
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    stage0 = np.array(14 * [3] + 4 * [2])
    ic = flopy.mf6.ModflowSwfic(swf, strt=stage0)

    dfw = flopy.mf6.ModflowSwfdfw(
        swf,
        central_in_space=True,
        print_flows=True,
        save_flows=True,
        width=1.0,  # cross sections defined explicitly (not fractions)
        manningsn=0.03,
        slope=0.001,
        idcxs=idcxs,
    )

    sto = flopy.mf6.ModflowSwfsto(
        swf,
        save_flows=True,
    )

    xfraction = (
        [0.0, 10.0, 20.0, 30.0]
        + [0, 20.0, 40.0, 60.0]
        + [0.0, 15.0, 30.0, 45.0]
    )
    height = (
        [10.0, 0.0, 0.0, 10.0]
        + [20.0, 0.0, 0.0, 20.0]
        + [15.0, 0.0, 0.0, 15.0]
    )
    mannfraction = (
        [1.0, 1.0, 1.0, 1.0] + [1.0, 1.0, 1.0, 1.0] + [1.0, 1.0, 1.0, 1.0]
    )

    cxsdata = list(zip(xfraction, height, mannfraction))
    cxs = flopy.mf6.ModflowSwfcxs(
        swf,
        nsections=3,
        npoints=4 * 3,
        packagedata=[(0, 4), (1, 4), (2, 4)],
        crosssectiondata=cxsdata,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{name}.bud",
        stage_filerecord=f"{name}.stage",
        saverecord=[
            ("STAGE", "ALL"),
            ("BUDGET", "ALL"),
        ],
        printrecord=[
            ("STAGE", "LAST"),
            ("BUDGET", "ALL"),
        ],
    )

    flwlist = [
        [(0,), "reach1"],
        [(4,), "reach5"],
    ]
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=len(flwlist),
        print_input=True,
        print_flows=True,
        stress_period_data=flwlist,
    )

    filename = name + ".flw.ts"
    time_series_namerecord = [("reach1", "reach5")]
    interpolation_methodrecord = [("linearend", "linearend")]
    flw.ts.initialize(
        filename=filename,
        timeseries=reach_inflow,
        time_series_namerecord=time_series_namerecord,
        interpolation_methodrecord=interpolation_methodrecord,
    )

    chd = flopy.mf6.ModflowSwfchd(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(17, 2.0)],
    )

    obs_data = {
        f"{name}.obs.csv": [
            ("REACH1", "STAGE", (0,)),
            ("REACH4", "STAGE", (3,)),
            ("REACH5", "STAGE", (4,)),
            ("REACH14", "STAGE", (13,)),
            ("REACH15", "STAGE", (14,)),
            ("REACH18", "STAGE", (17,)),
            ("FLOW45", "FLOW-JA-FACE", (3,), (4,)),
            ("FLOW56", "FLOW-JA-FACE", (4,), (5,)),
        ],
    }
    obs_package = flopy.mf6.ModflowUtlobs(
        swf,
        filename=f"{name}.obs",
        digits=10,
        print_input=True,
        continuous=obs_data,
    )

    return sim, None


def make_plot(test):
    print("making plots...")
    import matplotlib.pyplot as plt

    name = test.name
    ws = test.workspace
    mfsim = flopy.mf6.MFSimulation.load(sim_ws=ws)
    swf = mfsim.get_model(name)

    fpth = test.workspace / f"{name}.obs.csv"
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(1, 1, 1)
    for irch in [1, 4, 15, 18]:
        ax.plot(
            obsvals["time"],
            obsvals[f"REACH{irch}"],
            marker="o",
            mfc="none",
            mec="k",
            lw=0.0,
            label=f"MF6 reach {irch}",
        )
        ax.plot(
            obsvals["time"],
            answer[f"STAGE00000000{irch:02d}"],
            "k-",
            label=f"SWR Reach {irch}",
        )
    ax.set_xscale("log")
    plt.xlabel("time, in seconds")
    plt.ylabel("stage, in meters")
    plt.legend()
    fname = ws / f"{name}.obs.1.png"
    plt.savefig(fname)

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(
        obsvals["time"],
        obsvals["FLOW45"],
        marker="o",
        mfc="none",
        mec="b",
        lw=0.0,
        label="MF6 Gauge 4",
    )
    ax.plot(
        obsvals["time"],
        obsvals["FLOW56"],
        marker="o",
        mfc="none",
        mec="g",
        lw=0.0,
        label="MF6 Gauge 5",
    )
    ax.plot(
        answer_flow["TOTIME"], answer_flow["FLOW45"], "b-", label="SWR Gauge 4"
    )
    ax.plot(
        answer_flow["TOTIME"], answer_flow["FLOW56"], "g-", label="SWR Gauge 5"
    )
    # ax.plot(obsvals["time"], answer["STAGE0000000014"], marker="o", mfc="none", mec="k", lw=0., label="swr")
    ax.set_xscale("log")
    plt.xlabel("time, in seconds")
    plt.ylabel("flow, in cubic meters per second")
    plt.legend()
    fname = ws / f"{name}.obs.2.png"
    plt.savefig(fname)

    return


def check_output(idx, test):
    print("evaluating model...")

    makeplot = False
    if makeplot:
        make_plot(test)

    # read the observation output
    name = cases[idx]
    fpth = test.workspace / f"{name}.obs.csv"
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    diff = obsvals["REACH14"] - answer["STAGE0000000014"]
    print(diff)
    print(diff.max(), diff.min())
    assert np.allclose(
        diff, 0.0, atol=0.06
    ), f"Max diff with swr is {diff.min(), diff.max()}"

    # read the binary grid file
    fpth = test.workspace / f"{name}.disl.grb"
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read stage file
    fpth = test.workspace / f"{name}.stage"
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="STAGE")
    stage = qobj.get_alldata()

    # read the budget file
    fpth = test.workspace / f"{name}.bud"
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth)
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qextoutflow = budobj.get_data(text="CHD")

    # check budget terms
    for itime in range(len(flowja)):
        print(f"evaluating timestep {itime}")

        fja = flowja[itime].flatten()
        qresidual = fja[ia[:-1]]
        atol = 0.03
        for n in range(grb.nodes):
            passfail = "FAIL" if abs(qresidual[n]) > atol else ""
            print(
                f"residual for cell {n + 1} is {qresidual[n]} "
                f"in position {ia[n] + 1} {passfail}"
            )
        assert np.allclose(
            qresidual, 0.0, atol=atol
        ), f"residual in flowja diagonal is not zero"

    return


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
