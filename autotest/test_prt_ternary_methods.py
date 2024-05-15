"""
Tests a PRT model on the vertex grid demonstrated
at the end of Flopy's triangular mesh example:

https://flopy.readthedocs.io/en/latest/Notebooks/dis_triangle_example.html

There are two scenarios, both of which release
particles from the right border of the grid. In
the 1st case flow is left to right, in the 2nd
flow is top right to bottom left.

Runtime is benchmarked with pytest-benchmark.
The ZERO_METHOD option is used to select root-
finding methods for total runtime comparison.
"""

from pathlib import Path

import flopy
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
from flopy.discretization import VertexGrid
from flopy.utils import GridIntersect
from flopy.utils.triangle import Triangle
from framework import TestFramework
from prt_test_utils import get_model_name
from shapely.geometry import LineString
from test_prt_triangle import (
    active_domain,
    botm,
    build_gwf_sim,
    get_tri,
    nlay,
    porosity,
    top,
)

simname = "prtter"
cases = [
    f"{simname}br",
    f"{simname}ch",
]
methods = [
    1,  # brent
    2,  # chandrupatla
]


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
    flopy.mf6.ModflowPrtmip(
        prt, pname="mip", porosity=porosity, zero_method=methods[idx]
    )
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
        test.name, test.workspace, test.targets, ["left", "botm"]
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

    # check pathline shape and endpoints
    assert pls.shape == (116, 16)
    assert endpts.shape == (2, 16)
    assert set(endpts.icell) == {111, 112}


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets, benchmark):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
        compare=None,
    )
    benchmark(test.run)
