"""
Tests a PRT model on the Voronoi grid demonstrated
in Flopy's Voronoi example:

https://flopy.readthedocs.io/en/latest/Notebooks/dis_voronoi_example.html

Three variants are included, first with straight
left to right pathlines and no boundary conditions,
then again with wells, first pumping, then injection.
"""

from pathlib import Path

import flopy
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
from flopy.discretization import VertexGrid
from flopy.utils import GridIntersect
from flopy.utils.triangle import Triangle
from flopy.utils.voronoi import VoronoiGrid
from framework import TestFramework
from matplotlib.patches import Polygon
from modflow_devtools.markers import requires_pkg
from modflow_devtools.misc import is_in_ci
from prt_test_utils import get_model_name
from shapely.geometry import LineString, Point

pytest_plugins = ["modflow_devtools.snapshots"]

simname = "prtvor1"
cases = [f"{simname}l2r", f"{simname}welp", f"{simname}weli"]
times = [True, False, False]
tracktimes = list(np.linspace(0, 40000, 100))
xmin = 0.0
xmax = 2000.0
ymin = 0.0
ymax = 1000.0
top = 1.0
botm = [0.0]
angle_min = 30
area_max = 1000.0
delr = area_max**0.5
nlay = 1
ncol = xmax / delr
nrow = ymax / delr
nodes = ncol * nrow
porosity = 0.1
rpts = [[20, i, 0.5] for i in range(1, 999, 5)]


def get_grid(workspace, targets):
    workspace.mkdir(exist_ok=True, parents=True)
    tri = Triangle(
        maximum_area=area_max,
        angle=angle_min,
        model_ws=workspace,
        exe_name=targets["triangle"],
    )
    poly = np.array(((xmin, ymin), (xmax, ymin), (xmax, ymax), (xmin, ymax)))
    tri.add_polygon(poly)
    tri.build(verbose=False)
    return VoronoiGrid(tri)


def build_gwf_sim(name, ws, targets):
    ws = Path(ws)
    gwf_name = get_model_name(name, "gwf")

    # create grid
    grid = get_grid(ws / "grid", targets)
    vgrid = VertexGrid(**grid.get_gridprops_vertexgrid(), nlay=1)
    ibd = np.zeros(vgrid.ncpl, dtype=int)

    # If test changes the intersection needs to be recomputed
    # gi = GridIntersect(vgrid)

    # cells on left edge
    # line = LineString([(xmin, ymin), (xmin, ymax)])
    # cells_left = gi.intersect(line)["cellids"]
    left_cells = [
        0,
        3,
        1197,
        1268,
        1442,
        1443,
        1459,
        1461,
        1474,
        1499,
        1515,
        1520,
        1529,
        1545,
        1552,
        1563,
        1581,
        1584,
        1586,
        1590,
        1596,
        1608,
        1609,
        1614,
        1616,
        1625,
        1643,
        1649,
        1652,
        1655,
        1659,
        1662,
    ]
    left_cells = np.array(list(left_cells))
    ibd[left_cells] = 1

    # cells on right edge
    # line = LineString([(xmax, ymin), (xmax, ymax)])
    # cells_right = gi.intersect(line)["cellids"]
    right_cells = [
        1,
        2,
        6,
        12,
        210,
        399,
        400,
        406,
        412,
        421,
        519,
        559,
        601,
        605,
        617,
        623,
        624,
        674,
        770,
        783,
        785,
        786,
        792,
        793,
        801,
        809,
        812,
        813,
        814,
        920,
    ]
    right_cells = np.array(list(right_cells))
    ibd[right_cells] = 2

    # cells on bottom edge
    # line = LineString([(xmin, ymin), (xmax, ymin)])
    # cells_bottom = gi.intersect(line)["cellids"]
    bottom_cells = [
        0,
        1,
        4,
        8,
        258,
        274,
        308,
        328,
        332,
        333,
        334,
        342,
        343,
        345,
        347,
        353,
        354,
        355,
        439,
        445,
        456,
        460,
        465,
        475,
        479,
        485,
        516,
        527,
        533,
        541,
        631,
        752,
        810,
        819,
        824,
        830,
        832,
        834,
        835,
        927,
        929,
        932,
        1091,
        1096,
        1207,
        1212,
        1215,
        1217,
        1242,
        1247,
        1249,
        1257,
        1262,
        1269,
        1276,
        1389,
        1393,
        1447,
        1455,
        1462,
        1677,
        1685,
    ]
    bottom_cells = np.array(list(bottom_cells))
    ibd[bottom_cells] = 3

    # well cells
    # points = [Point((1200, 500)), Point((700, 200)), Point((1600, 700))]
    # well_cells = [vgrid.intersect(p.x, p.y) for p in points]
    well_cells = [163, 1178, 67]

    # create simulation
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=targets["mf6"], sim_ws=ws
    )
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", perioddata=[[1.0, 1, 1.0]]
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwf_name, save_flows=True)
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="complex",
        outer_dvclose=1.0e-8,
        inner_dvclose=1.0e-8,
    )
    disv = flopy.mf6.ModflowGwfdisv(
        gwf, nlay=nlay, **grid.get_disv_gridprops(), top=top, botm=botm
    )
    if "wel" in name:
        # k, j, q
        wells = [
            (0, c, 0.5 * (-1 if "welp" in name else 1)) for c in well_cells
        ]
        wel = flopy.mf6.ModflowGwfwel(
            gwf,
            maxbound=len(wells),
            save_flows=True,
            stress_period_data={0: wells},
        )
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=[(True)],
        k=10.0,
        save_saturation=True,
        save_specific_discharge=True,
    )
    ic = flopy.mf6.ModflowGwfic(gwf)

    chdlist = []
    icpl_seen = []
    for icpl in left_cells:
        chdlist.append([(0, icpl), 1.0])
        icpl_seen.append(icpl)
    for icpl in right_cells:
        chdlist.append([(0, icpl), 0.0])
        icpl_seen.append(icpl)
    if "wel" in name:
        for icpl in bottom_cells:
            if icpl in icpl_seen:
                continue
            chdlist.append([(0, icpl), 0.8])
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chdlist)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwf_name}.bud",
        head_filerecord=f"{gwf_name}.hds",
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )
    return sim, {
        "left": left_cells,
        "right": right_cells,
        "bottom": bottom_cells,
        "well": well_cells,
    }


def build_prt_sim(idx, name, gwf_ws, prt_ws, targets, cell_ids):
    prt_ws = Path(prt_ws)
    gwf_name = get_model_name(name, "gwf")
    prt_name = get_model_name(name, "prt")

    # create grid
    grid = get_grid(prt_ws / "grid", targets)
    gridprops = grid.get_gridprops_vertexgrid()
    vgrid = VertexGrid(**gridprops, nlay=1)
    ibd = np.zeros(vgrid.ncpl, dtype=int)

    # cells on left edge
    left_cells = cell_ids["left"]
    left_cells = np.array(list(left_cells))
    ibd[left_cells] = 1

    # cells on right edge
    right_cells = cell_ids["right"]
    right_cells = np.array(list(right_cells))
    ibd[right_cells] = 2

    # create simulation
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=targets["mf6"], sim_ws=prt_ws
    )
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", perioddata=[[1.0, 1, 1.0]]
    )
    prt = flopy.mf6.ModflowPrt(sim, modelname=prt_name)
    disv = flopy.mf6.ModflowGwfdisv(
        prt, nlay=nlay, **grid.get_disv_gridprops(), top=top, botm=botm
    )
    flopy.mf6.ModflowPrtmip(prt, pname="mip", porosity=porosity)

    prpdata = [
        # index, (layer, cell index), x, y, z
        (i, (0, vgrid.intersect(p[0], p[1])), p[0], p[1], p[2])
        for i, p in enumerate(rpts)  # first release point crashes
    ]
    prp_track_file = f"{prt_name}.prp.trk"
    prp_track_csv_file = f"{prt_name}.prp.trk.csv"
    flopy.mf6.ModflowPrtprp(
        prt,
        pname="prp1",
        filename=f"{prt_name}_1.prp",
        nreleasepts=len(prpdata),
        packagedata=prpdata,
        perioddata={0: ["FIRST"]},
        track_filerecord=[prp_track_file],
        trackcsv_filerecord=[prp_track_csv_file],
        boundnames=True,
        stop_at_weak_sink=True,
    )
    prt_track_file = f"{prt_name}.trk"
    prt_track_csv_file = f"{prt_name}.trk.csv"
    flopy.mf6.ModflowPrtoc(
        prt,
        pname="oc",
        track_filerecord=[prt_track_file],
        trackcsv_filerecord=[prt_track_csv_file],
        track_all=not times[idx],
        track_transit=True,
        track_release=True,
        track_terminate=True,
        # track_usertime=times[idx],
        # track_timesrecord=tracktimes if times[idx] else None,
    )
    gwf_budget_file = gwf_ws / f"{gwf_name}.bud"
    gwf_head_file = gwf_ws / f"{gwf_name}.hds"
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
        filename=f"{prt_name}.ems",
    )
    sim.register_solution_package(ems, [prt.name])
    return sim


def build_models(idx, test):
    gwf_sim, cell_ids = build_gwf_sim(test.name, test.workspace, test.targets)
    prt_sim = build_prt_sim(
        idx,
        test.name,
        test.workspace,
        test.workspace / "prt",
        test.targets,
        cell_ids,
    )
    return gwf_sim, prt_sim


def plot_output(name, gwf, head, spdis, pls, fpath):
    # plot in 2d with mpl
    fig = plt.figure(figsize=(16, 10))
    ax = plt.subplot(1, 1, 1, aspect="equal")
    pmv = flopy.plot.PlotMapView(model=gwf, ax=ax)
    pmv.plot_grid(alpha=0.25)
    pmv.plot_ibound(alpha=0.5)
    headmesh = pmv.plot_array(head, alpha=0.25)
    cv = pmv.contour_array(head, levels=np.linspace(0, 1, 9), colors="black")
    plt.clabel(cv)
    plt.colorbar(headmesh, shrink=0.25, ax=ax, label="Head", location="right")
    handles = [
        mpl.lines.Line2D(
            [0],
            [0],
            marker=">",
            linestyle="",
            label="Specific discharge",
            color="grey",
            markerfacecolor="gray",
        ),
    ]
    if "wel" in name:
        handles.append(
            mpl.lines.Line2D(
                [0],
                [0],
                marker="o",
                linestyle="",
                label="Well",
                markerfacecolor="red",
            ),
        )
    ax.legend(
        handles=handles,
        loc="lower right",
    )
    pmv.plot_vector(*spdis, normalize=True, alpha=0.25)
    if "wel" in name:
        pmv.plot_bc(ftype="WEL")
    mf6_plines = pls.groupby(["iprp", "irpt", "trelease"])
    for ipl, ((iprp, irpt, trelease), pl) in enumerate(mf6_plines):
        title = "DISV voronoi grid particle tracks"
        if "welp" in name:
            title += ": pumping wells"
        elif "weli" in name:
            title += ": injection wells"
        pl.plot(
            title=title,
            kind="line",
            linestyle="--",
            marker="o",
            markersize=2,
            x="x",
            y="y",
            ax=ax,
            legend=False,
            color="black",
        )
    xc, yc = gwf.modelgrid.get_xcellcenters_for_layer(
        0
    ), gwf.modelgrid.get_ycellcenters_for_layer(0)
    for i in range(gwf.modelgrid.ncpl):
        x, y = xc[i], yc[i]
        if i == 1639:
            color = "green"
            ms = 10
        else:
            color = "grey"
            ms = 2
        ax.plot(x, y, "o", color=color, alpha=0.25, ms=ms)
        ax.annotate(str(i + 1), (x, y), color="grey", alpha=0.5)

    plt.show()
    plt.savefig(fpath)

    # plot in 3d with pyvista (via vtk)
    import pyvista as pv
    from flopy.export.vtk import Vtk
    from flopy.plot.plotutil import to_mp7_pathlines

    def get_meshes(model, pathlines):
        vtk = Vtk(model=model, binary=False, smooth=False)
        vtk.add_model(model)
        vtk.add_pathline_points(
            to_mp7_pathlines(pathlines.to_records(index=False))
        )
        grid_mesh, path_mesh = vtk.to_pyvista()
        grid_mesh.rotate_x(-100, point=axes.origin, inplace=True)
        grid_mesh.rotate_z(90, point=axes.origin, inplace=True)
        grid_mesh.rotate_y(120, point=axes.origin, inplace=True)
        path_mesh.rotate_x(-100, point=axes.origin, inplace=True)
        path_mesh.rotate_z(90, point=axes.origin, inplace=True)
        path_mesh.rotate_y(120, point=axes.origin, inplace=True)
        return grid_mesh, path_mesh

    def callback(mesh, value):
        sub = pls[pls.t <= value]
        gm, pm = get_meshes(gwf, sub)
        mesh.shallow_copy(pm)

    pv.set_plot_theme("document")
    axes = pv.Axes(show_actor=True, actor_scale=2.0, line_width=5)
    p = pv.Plotter(notebook=False)
    grid_mesh, path_mesh = get_meshes(gwf, pls)
    p.add_mesh(grid_mesh, scalars=head[0], cmap="Blues", opacity=0.5)
    p.add_mesh(path_mesh, label="Time", style="points", color="black")
    p.camera.zoom(1)
    p.add_slider_widget(lambda v: callback(path_mesh, v), [0, 30202])
    p.show()


def check_output(idx, test, snapshot):
    name = test.name
    prt_ws = test.workspace / "prt"
    prt_name = get_model_name(name, "prt")
    gwfsim = test.sims[0]

    # get gwf output
    gwf = gwfsim.get_model()
    head = gwf.output.head().get_data()
    bdobj = gwf.output.budget()
    spdis = bdobj.get_data(text="DATA-SPDIS")[0]
    qx, qy, _ = flopy.utils.postprocessing.get_specific_discharge(spdis, gwf)

    # get prt output
    prt_track_csv_file = f"{prt_name}.prp.trk.csv"
    pls = pd.read_csv(prt_ws / prt_track_csv_file, na_filter=False)
    endpts = pls[pls.ireason == 3]  # termination

    # compare pathlines with snapshot
    assert snapshot == endpts.round(1 if "weli" in name else 2).to_records(
        index=False
    )

    # plot results if enabled
    plot = False
    if plot:
        plot_output(
            name, gwf, head, (qx, qy), pls, fpath=prt_ws / f"{name}.png"
        )


@requires_pkg("syrupy")
@pytest.mark.slow
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(
    idx, name, function_tmpdir, targets, benchmark, array_snapshot
):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t, array_snapshot),
        targets=targets,
        compare=None,
    )
    benchmark(test.run)
