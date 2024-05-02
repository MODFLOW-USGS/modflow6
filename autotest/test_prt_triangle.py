"""
Tests a PRT model on the vertex grid demonstrated
at the end of Flopy's triangular mesh example:

https://flopy.readthedocs.io/en/latest/Notebooks/dis_triangle_example.html

There are two scenarios, both of which release
particles from the right border of the grid. In
the 1st case flow is left to right, in the 2nd
flow is top right to bottom left.
"""

from math import isclose
from pathlib import Path

import flopy
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
from flopy.discretization import VertexGrid
from flopy.utils import GridIntersect
from flopy.utils.triangle import Triangle
from shapely.geometry import LineString

from framework import TestFramework
from prt_test_utils import get_model_name

simname = "prttri"
cases = [f"{simname}r2l", f"{simname}diag"]
angle = 30
max_area = 100
active_domain = [(0, 0), (100, 0), (100, 100), (0, 100)]
nlay = 1
top = 1.0
botm = [0.0]
k = 10.0
tdis_rc = [[1.0, 1, 1.0]]
porosity = 0.1


def get_chd_head(x):
    return x * 10.0 / 100.0


def get_tri(workspace, targets) -> Triangle:
    workspace.mkdir(exist_ok=True, parents=True)
    tri = Triangle(
        angle=angle,
        maximum_area=max_area,
        model_ws=workspace,
        exe_name=targets["triangle"],
    )
    tri.add_polygon(active_domain)
    tri.build()
    return tri


def build_gwf_sim(name, ws, targets, chd_sides=None):
    ws = Path(ws)
    gwfname = get_model_name(name, "gwf")
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=targets["mf6"], sim_ws=ws
    )
    tdis = flopy.mf6.ModflowTdis(sim, time_units="DAYS", perioddata=tdis_rc)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="complex",
        outer_dvclose=1.0e-8,
        inner_dvclose=1.0e-8,
    )
    tri = get_tri(ws / "grid", targets)
    cell2d = tri.get_cell2d()
    vertices = tri.get_vertices()
    xcyc = tri.get_xcyc()
    ncpl = tri.ncpl
    nvert = tri.nvert
    dis = flopy.mf6.ModflowGwfdisv(
        gwf,
        nlay=nlay,
        ncpl=ncpl,
        nvert=nvert,
        top=top,
        botm=botm,
        vertices=vertices,
        cell2d=cell2d,
    )
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=[(True)],
        save_specific_discharge=True,
        save_saturation=True,
    )
    ic = flopy.mf6.ModflowGwfic(gwf)
    cells = []
    chdlist = []

    if isinstance(chd_sides, (list, tuple)):
        if "left" in chd_sides:
            leftcells = tri.get_edge_cells(4)
            leftcells = set(leftcells) - set(cells)
            cells.extend(leftcells)
        if "right" in chd_sides:
            rightcells = tri.get_edge_cells(2)
            rightcells = set(rightcells) - set(cells)
            cells.extend(rightcells)
        if "botm" in chd_sides:
            botmcells = tri.get_edge_cells(3)
            botmcells = set(botmcells) - set(cells)
            cells.extend(botmcells)
        if "top" in chd_sides:
            topcells = tri.get_edge_cells(1)
            topcells = set(topcells) - set(cells)
            cells.extend(topcells)
        for icpl in set(cells):
            h = get_chd_head(xcyc[icpl, 0])
            chdlist.append([(0, icpl), h])

        iflowface_map = {89: 3, 87: 2, 130: 3, 136: 3, 112: 3, 111: 3}

        def add_iflowface(row):
            ic = row[0][1] + 1  # 0 to 1-based indexing
            iff = iflowface_map.get(ic, -1)
            return (*row, iff)

        chdlist = [add_iflowface(row) for row in chdlist]

    chd = flopy.mf6.ModflowGwfchd(
        gwf, stress_period_data=chdlist, auxiliary=["IFLOWFACE"]
    )
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )
    return sim


def build_prt_sim(idx, name, gwf_ws, prt_ws, targets):
    prt_ws = Path(prt_ws)
    gwfname = get_model_name(name, "gwf")
    prtname = get_model_name(name, "prt")

    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=targets["mf6"], sim_ws=prt_ws
    )
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", perioddata=[[1.0, 1, 1.0]]
    )
    prt = flopy.mf6.ModflowPrt(sim, modelname=prtname)
    tri = get_tri(prt_ws / "grid", targets)
    cell2d = tri.get_cell2d()
    vertices = tri.get_vertices()
    xcyc = tri.get_xcyc()
    ncpl = tri.ncpl
    nvert = tri.nvert
    dis = flopy.mf6.ModflowGwfdisv(
        prt,
        nlay=nlay,
        ncpl=ncpl,
        nvert=nvert,
        top=top,
        botm=botm,
        vertices=vertices,
        cell2d=cell2d,
    )
    flopy.mf6.ModflowPrtmip(prt, pname="mip", porosity=porosity)
    prpdata = [
        # particle index, (layer, cell index), x, y, z
        (0, (0, 88), 95, 92, 0.5),
        (1, (0, 86), 96, 86, 0.5),
    ]
    prp_track_file = f"{prtname}.prp.trk"
    prp_track_csv_file = f"{prtname}.prp.trk.csv"
    flopy.mf6.ModflowPrtprp(
        prt,
        pname="prp1",
        filename=f"{prtname}_1.prp",
        nreleasepts=len(prpdata),
        packagedata=prpdata,
        perioddata={0: ["FIRST"]},
        track_filerecord=[prp_track_file],
        trackcsv_filerecord=[prp_track_csv_file],
        boundnames=True,
        stop_at_weak_sink=True,  # currently required for this problem
    )
    prt_track_file = f"{prtname}.trk"
    prt_track_csv_file = f"{prtname}.trk.csv"
    flopy.mf6.ModflowPrtoc(
        prt,
        pname="oc",
        track_filerecord=[prt_track_file],
        trackcsv_filerecord=[prt_track_csv_file],
    )
    gwf_budget_file = gwf_ws / f"{gwfname}.cbc"
    gwf_head_file = gwf_ws / f"{gwfname}.hds"
    flopy.mf6.ModflowPrtfmi(
        prt,
        packagedata=[
            ("GWFHEAD", gwf_head_file),
            ("GWFBUDGET", gwf_budget_file),
        ],
    )
    ems = flopy.mf6.ModflowEms(
        sim,
        pname="ems",
        filename=f"{prtname}.ems",
    )
    sim.register_solution_package(ems, [prt.name])
    return sim


def build_models(idx, test):
    gwf_sim = build_gwf_sim(
        test.name,
        test.workspace,
        test.targets,
        (
            ["left", "right"]
            if "r2l" in test.name
            else ["left", "botm"] if "diag" in test.name else None
        ),
    )
    prt_sim = build_prt_sim(
        idx, test.name, test.workspace, test.workspace / "prt", test.targets
    )
    return gwf_sim, prt_sim


def check_output(idx, test):
    name = test.name
    prt_ws = test.workspace / "prt"
    gwf_name = get_model_name(name, "gwf")
    prt_name = get_model_name(name, "prt")
    gwf_sim = test.sims[0]
    gwf = gwf_sim.get_model(gwf_name)

    # get gwf output
    gwf = gwf_sim.get_model()
    head = gwf.output.head().get_data()
    bdobj = gwf.output.budget()
    spdis = bdobj.get_data(text="DATA-SPDIS")[0]
    qx, qy, qz = flopy.utils.postprocessing.get_specific_discharge(spdis, gwf)

    # get prt output
    prt_name = get_model_name(name, "prt")
    prt_track_csv_file = f"{prt_name}.prp.trk.csv"
    pls = pd.read_csv(prt_ws / prt_track_csv_file, na_filter=False)
    endpts = (
        pls.sort_values("t")
        .groupby(["imdl", "iprp", "irpt", "trelease"])
        .tail(1)
    )

    plot_debug = False
    if plot_debug:
        fig = plt.figure(figsize=(10, 10))
        ax = plt.subplot(1, 1, 1, aspect="equal")
        pmv = flopy.plot.PlotMapView(model=gwf, ax=ax)
        pmv.plot_grid()
        pmv.plot_array(head, cmap="Blues", alpha=0.25)
        pmv.plot_vector(qx, qy, normalize=True, alpha=0.25)
        mf6_plines = pls.groupby(["iprp", "irpt", "trelease"])
        for ipl, ((iprp, irpt, trelease), pl) in enumerate(mf6_plines):
            pl.plot(
                title=f"MF6 pathlines ({name})",
                kind="line",
                x="x",
                y="y",
                ax=ax,
                legend=False,
                color="blue",
            )
        grid = gwf.modelgrid
        xc, yc = grid.get_xcellcenters_for_layer(
            0
        ), grid.get_ycellcenters_for_layer(0)
        for i in range(grid.ncpl):
            x, y = xc[i], yc[i]
            ax.plot(x, y, "o", color="grey", alpha=0.25, ms=2)
            ax.annotate(str(i + 1), (x, y), color="grey", alpha=0.5)
        plt.show()

    if "r2l" in name:
        assert pls.shape == (82, 16)
        assert (pls.z == 0.5).all()
        rtol = 1e-6
        assert isclose(min(pls.x), 0, rel_tol=rtol)
        assert isclose(max(pls.x), 96, rel_tol=rtol)
        assert isclose(min(pls[pls.irpt == 1].y), 91.690657, rel_tol=rtol)
        assert isclose(max(pls[pls.irpt == 1].y), 92, rel_tol=rtol)
        assert isclose(min(pls[pls.irpt == 2].y), 85.585069, rel_tol=rtol)
        assert isclose(max(pls[pls.irpt == 2].y), 86, rel_tol=rtol)
        assert set(endpts.icell) == {130, 136}
    elif "diag" in name:
        assert pls.shape == (116, 16)
        assert endpts.shape == (2, 16)
        assert set(endpts.icell) == {111, 112}


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
        compare=None,
    )
    test.run()
