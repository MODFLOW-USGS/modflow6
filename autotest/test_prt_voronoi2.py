"""
Tests a PRT model on the Voronoi grid demonstrated
in Flopy's Voronoi example:

https://flopy.readthedocs.io/en/latest/Notebooks/dis_voronoi_example.html

Particles are released from the center of the plume
(i.e. the constant concentration cell) used in the
transport model.

TODO: support parallel adjacent cell faces,
duplicated vertices as flopy.utils.voronoi
can produce via scipy/Qhull (for now flopy
filters these but mf6 probably should too)
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
from prt_test_utils import get_model_name
from shapely.geometry import LineString, Point

from framework import TestFramework

simname = "prtvor2"
cases = [simname]
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
    gi = GridIntersect(vgrid)

    # identify cells on left edge
    line = LineString([(xmin, ymin), (xmin, ymax)])
    cells_left = gi.intersect(line)["cellids"]
    cells_left = np.array(list(cells_left))
    ibd[cells_left] = 1

    # identify cells on right edge
    line = LineString([(xmax, ymin), (xmax, ymax)])
    cells_right = gi.intersect(line)["cellids"]
    cells_right = np.array(list(cells_right))
    ibd[cells_right] = 2

    # identify cells on bottom edge
    line = LineString([(xmin, ymin), (xmax, ymin)])
    cells_bottom = gi.intersect(line)["cellids"]
    cells_bottom = np.array(list(cells_bottom))
    ibd[cells_bottom] = 3

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
    for icpl in cells_left:
        chdlist.append([(0, icpl), 1.0])
        icpl_seen.append(icpl)
    for icpl in cells_right:
        chdlist.append([(0, icpl), 0.0])
        icpl_seen.append(icpl)
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chdlist)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwf_name}.bud",
        head_filerecord=f"{gwf_name}.hds",
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )
    return sim


def build_gwt_sim(name, gwf_ws, gwt_ws, targets):
    ws = Path(gwt_ws)
    gwf_name = get_model_name(name, "gwf")
    gwt_name = get_model_name(name, "gwt")

    # create grid
    grid = get_grid(ws / "grid", targets)
    vgrid = VertexGrid(**grid.get_gridprops_vertexgrid(), nlay=1)
    ibd = np.zeros(vgrid.ncpl, dtype=int)
    gi = GridIntersect(vgrid)

    # identify release cell
    point = Point((500, 500))
    cells2 = gi.intersect(point)["cellids"]
    cells2 = np.array(list(cells2))

    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", perioddata=[[100 * 365.0, 100, 1.0]]
    )
    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwt_name, save_flows=True)
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="simple",
        linear_acceleration="bicgstab",
        outer_dvclose=1.0e-6,
        inner_dvclose=1.0e-6,
    )
    disv_gridprops = grid.get_disv_gridprops()
    nlay = 1
    top = 1.0
    botm = [0.0]
    disv = flopy.mf6.ModflowGwtdisv(
        gwt, nlay=nlay, **disv_gridprops, top=top, botm=botm
    )
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0)
    sto = flopy.mf6.ModflowGwtmst(gwt, porosity=0.2)
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme="TVD")
    dsp = flopy.mf6.ModflowGwtdsp(gwt, alh=5.0, ath1=0.5)
    sourcerecarray = [()]
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)
    cnclist = [
        [(0, cells2[0]), 1.0],
    ]
    cnc = flopy.mf6.ModflowGwtcnc(
        gwt, maxbound=len(cnclist), stress_period_data=cnclist, pname="CNC-1"
    )
    gwf_budget_file = gwf_ws / f"{gwf_name}.bud"
    gwf_head_file = gwf_ws / f"{gwf_name}.hds"
    flopy.mf6.ModflowGwtfmi(
        gwt,
        packagedata=[
            ("GWFHEAD", gwf_head_file),
            ("GWFBUDGET", gwf_budget_file),
        ],
    )
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{name}.cbc",
        concentration_filerecord=f"{name}.ucn",
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )

    return sim


def build_prt_sim(name, gwf_ws, prt_ws, targets):
    prt_ws = Path(prt_ws)
    gwf_name = get_model_name(name, "gwf")
    prt_name = get_model_name(name, "prt")

    # create grid
    grid = get_grid(prt_ws / "grid", targets)
    gridprops = grid.get_gridprops_vertexgrid()
    vgrid = VertexGrid(**gridprops, nlay=1)
    ibd = np.zeros(vgrid.ncpl, dtype=int)
    gi = GridIntersect(vgrid)

    # identify cells on left edge
    line = LineString([(xmin, ymin), (xmin, ymax)])
    cells0 = gi.intersect(line)["cellids"]
    cells0 = np.array(list(cells0))
    ibd[cells0] = 1

    # identify cells on right edge
    line = LineString([(xmax, ymin), (xmax, ymax)])
    cells1 = gi.intersect(line)["cellids"]
    cells1 = np.array(list(cells1))
    ibd[cells1] = 2

    # identify release cell
    point = Point((500, 500))
    cells2 = gi.intersect(point)["cellids"]
    cells2 = np.array(list(cells2))

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

    sddata = flopy.modpath.CellDataType(
        columncelldivisions=1, rowcelldivisions=1
    )
    data = flopy.modpath.NodeParticleData(
        subdivisiondata=sddata, nodes=[cells2]
    )
    prpdata = list(data.to_prp(prt.modelgrid))
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
        stop_at_weak_sink=True,  # currently required for this problem
    )
    prt_track_file = f"{prt_name}.trk"
    prt_track_csv_file = f"{prt_name}.trk.csv"
    flopy.mf6.ModflowPrtoc(
        prt,
        pname="oc",
        track_filerecord=[prt_track_file],
        trackcsv_filerecord=[prt_track_csv_file],
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
    gwf_sim = build_gwf_sim(test.name, test.workspace, test.targets)
    gwt_sim = build_gwt_sim(
        test.name, test.workspace, test.workspace / "gwt", test.targets
    )
    prt_sim = build_prt_sim(
        test.name, test.workspace, test.workspace / "prt", test.targets
    )
    return gwf_sim, gwt_sim, prt_sim


def check_output(idx, test):
    name = test.name
    prt_ws = test.workspace / "prt"
    prt_name = get_model_name(name, "prt")
    gwfsim, gwtsim, prtsim = test.sims

    # get gwf output
    gwf = gwfsim.get_model()
    head = gwf.output.head().get_data()
    bdobj = gwf.output.budget()
    spdis = bdobj.get_data(text="DATA-SPDIS")[0]
    qx, qy, qz = flopy.utils.postprocessing.get_specific_discharge(spdis, gwf)

    # get gwt output
    gwt = gwtsim.get_model()
    conc = gwt.output.concentration().get_data()

    # get prt output
    prt_track_csv_file = f"{prt_name}.prp.trk.csv"
    pls = pd.read_csv(prt_ws / prt_track_csv_file, na_filter=False)

    plot_2d = False
    if plot_2d:
        # plot in 2d with mpl
        fig = plt.figure(figsize=(16, 10))
        ax = plt.subplot(1, 1, 1, aspect="equal")
        pmv = flopy.plot.PlotMapView(model=gwf, ax=ax)
        pmv.plot_grid(alpha=0.25)
        pmv.plot_ibound(alpha=0.5)
        # headmesh = pmv.plot_array(head, alpha=0.25)
        # headctr = pmv.contour_array(head, levels=np.linspace(0, 1, 9), colors="black")
        # plt.clabel(headctr)
        # plt.colorbar(headmesh, shrink=0.25, ax=ax, label="Head", location="right")
        concmesh = pmv.plot_array(conc, cmap="jet")
        concctr = pmv.contour_array(
            conc, levels=(0.0001, 0.001, 0.01, 0.1), colors="y"
        )
        plt.clabel(concctr)
        plt.colorbar(
            concmesh,
            shrink=0.25,
            ax=ax,
            label="Concentration",
            location="right",
        )

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
        ax.legend(
            handles=handles,
            loc="lower right",
        )
        pmv.plot_vector(qx, qy, normalize=True, alpha=0.25)
        mf6_plines = pls.groupby(["iprp", "irpt", "trelease"])
        for ipl, ((iprp, irpt, trelease), pl) in enumerate(mf6_plines):
            title = "DISV voronoi grid particle tracks"
            pl.plot(
                title=title,
                kind="line",
                x="x",
                y="y",
                ax=ax,
                legend=False,
                color="black",
            )
        plt.show()
        plt.savefig(prt_ws / f"{name}.png")

    plot_3d = False
    if plot_3d:
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


@pytest.mark.slow
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
