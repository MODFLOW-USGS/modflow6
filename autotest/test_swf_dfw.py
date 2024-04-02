"""

Simple 3 reach network with 4 vertices


zero-based diagram below

o------o------o------o
v0     v1     v2     v3
    r0     r1     r2

ia  ja
0   0 1
2   1 0 2
5   2 1
7

"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = [
    "swf-dfw01",
]


def build_models(idx, test):

    sim_ws = test.workspace
    name = cases[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=sim_ws,
        memory_print_option="all",
    )

    tdis = flopy.mf6.ModflowTdis(sim)
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="all",
        linear_acceleration="BICGSTAB",
        outer_dvclose=1.0e-7,
        inner_dvclose=1.0e-8,
    )
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True, print_flows=True)

    dx = 1000.0
    nreach = 3
    total_length = dx * nreach
    vertices = []
    vertices = [[j, j * dx, 0.0] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    nodes = len(cell2d)
    nvert = len(vertices)

    disv1d = flopy.mf6.ModflowSwfdisv1D(
        swf,
        nodes=nodes,
        nvert=nvert,
        length=dx,
        width=50.,
        bottom=0.0,
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    dfw = flopy.mf6.ModflowSwfdfw(
        swf,
        print_flows=True,
        save_flows=True,
        manningsn=0.035,
        idcxs=0,
    )

    sto = flopy.mf6.ModflowSwfsto(
        swf,
        save_flows=True,
    )

    ic = flopy.mf6.ModflowSwfic(swf, strt=1.0)

    xfraction = [0.0, 0.0, 1.0, 1.0]
    height = [100.0, 0.0, 0.0, 100.0]
    mannfraction = [1.0, 1.0, 1.0, 1.0]
    cxsdata = list(zip(xfraction, height, mannfraction))
    cxs = flopy.mf6.ModflowSwfcxs(
        swf,
        nsections=1,
        npoints=4,
        packagedata=[(0, 4)],
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

    # Save to external binary file or into flw package depending on binary keyword
    binary = True
    flw_list = [
        (1, 100),
    ]  # one-based cell numbers here
    maxbound = len(flw_list)
    if binary:
        ra = np.array(flw_list, dtype=[("irch", "<i4"), ("q", "<f8")])
        ra.tofile(os.path.join(sim_ws, "flw0.bin"))
        flw_spd = {
            0: {
                "filename": "flw0.bin",
                "binary": True,
                "data": None,
            },
        }
    else:
        flw_spd = {0: flw_list}
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=maxbound,
        print_input=True,
        print_flows=True,
        stress_period_data=flw_spd,
    )

    chd = flopy.mf6.ModflowSwfchd(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(2, 1.0)],
    )

    return sim, None


def check_output(idx, test):
    print("evaluating model...")

    # assign name
    name = cases[idx]

    # read the binary grid file
    fpth = test.workspace / f"{name}.disv1d.grb"
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
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
    qchd = budobj.get_data(text="CHD")
    qresidual = np.zeros(grb.nodes)

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
