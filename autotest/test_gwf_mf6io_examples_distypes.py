import pathlib as pl

import flopy
import numpy as np
import pytest
from flopy.utils.gridgen import Gridgen

from framework import TestFramework
from simulation import TestSimulation

dis_types = (
    "dis",
    "disv",
    "disu",
)
problems = (
    "ps1a",
    "ps2a",
    "ps2a_list",
    "ps2b",
)
ex = []
for problem in problems:
    ex += [f"{problem}_{dis_type}" for dis_type in dis_types]
# remove invalid combinations
for tag in ("ps2a_disu",):
    ex.remove(tag)

paktest = "csub"

# base spatial discretization
nlay, nrow, ncol = 3, 21, 20
shape3d = (nlay, nrow, ncol)
shape2d = (nrow, ncol)
size3d = nlay * nrow * ncol
size2d = nrow * ncol

delr = delc = 500.0
x0_base, x1_base, y0_base, y1_base = 0, ncol * delr, 0.0, nrow * delc
top = 400.0
bot = 0.0
botm = [220.0, 200.0, 0.0]
z_node = [310.0, 210.0, 100.0]

hk = [50.0, 0.01, 200.0]
vk = [10.0, 0.01, 20.0]

canal_head = 330.0
canal_coordinates = [
    (0.5 * delr, y1_base - delr * (i + 0.5)) for i in range(nrow)
]

river_head = 320.0
river_coordinates = [
    (x1_base - 0.5 * delr, y1_base - delr * (i + 0.5)) for i in range(nrow)
]

rch_rate = 0.005


def build_dis(gwf):
    return flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )


def get_gridgen_ws(ws):
    gridgen_ws = ws / "gridgen"
    gridgen_ws.mkdir(parents=True, exist_ok=True)
    return gridgen_ws


def build_temp_gwf(ws):
    gridgen_ws = get_gridgen_ws(ws)
    gridgen_sim = flopy.mf6.MFSimulation(
        sim_name="gridgen", sim_ws=gridgen_ws, exe_name="mf6"
    )
    gridgen_gwf = flopy.mf6.ModflowGwf(gridgen_sim, modelname="gridgen")
    return gridgen_gwf


def build_disv(ws, gwf, gridgen):
    temp_gwf = build_temp_gwf(ws)
    dis = build_dis(temp_gwf)
    g = Gridgen(
        temp_gwf.modelgrid,
        model_ws=get_gridgen_ws(ws),
        exe_name=gridgen,
    )
    g.build()
    gridprops = g.get_gridprops_disv()
    return flopy.mf6.ModflowGwfdisv(gwf, **gridprops)


def build_disu(ws, gwf, gridgen):
    temp_gwf = build_temp_gwf(ws)
    dis = build_dis(temp_gwf)
    g = Gridgen(
        temp_gwf.modelgrid,
        model_ws=get_gridgen_ws(ws),
        exe_name=gridgen,
    )
    g.build()
    gridprops = g.get_gridprops_disu6()
    return flopy.mf6.ModflowGwfdisu(gwf, **gridprops)


def get_node_number(modelgrid, cellid):
    if modelgrid.grid_type == "unstructured":
        node = cellid
    elif modelgrid.grid_type == "vertex":
        node = modelgrid.ncpl * cellid[0] + cellid[1]
    else:
        node = (
            modelgrid.nrow * modelgrid.ncol * cellid[0]
            + modelgrid.ncol * cellid[1]
            + cellid[2]
        )
    return node


def build_3d_array(modelgrid, values, dtype=float):
    if isinstance(values, dtype):
        arr = np.full(modelgrid.nnodes, values, dtype=dtype)
    else:
        arr = np.zeros(modelgrid.nnodes, dtype=dtype)
        ia = []
        x0, x1, y0, y1 = modelgrid.extent
        for k in range(nlay):
            cellid = modelgrid.intersect(x0 + 0.1, y1 - 0.1, z=z_node[k])
            ia.append(get_node_number(modelgrid, cellid))
        ia.append(modelgrid.nnodes + 1)
        for k in range(nlay):
            arr[ia[k] : ia[k + 1]] = values[k]
    return arr.reshape(modelgrid.shape)


def build_chd_data(modelgrid, coordinates, head, layer_number=0):
    chd_spd = []
    for x, y in coordinates:
        cellid = modelgrid.intersect(x, y, z=z_node[layer_number])
        if isinstance(cellid, tuple):
            chd_spd.append((*cellid, head))
        else:
            chd_spd.append((cellid, head))
    return {0: chd_spd}


def build_riv_data(modelgrid):
    cond = 20.0 * 10 * delc / 1.0
    spd = []
    for x, y in river_coordinates:
        cellid = modelgrid.intersect(x, y, z=z_node[0])
        if isinstance(cellid, int):
            cellid = (cellid,)
        spd.append((*cellid, river_head, cond, 317.0))
    return {0: spd}


def build_rch_package(gwf, name):
    if "list" in name or name.endswith("disu"):
        spd = []
        for i in range(nrow):
            y = y1_base - delr * (i + 0.5)
            for j in range(ncol):
                x = delc * (j + 0.5)
                cellid = gwf.modelgrid.intersect(x, y, z=z_node[0])
                if isinstance(cellid, int):
                    cellid = (cellid,)
                spd.append((*cellid, rch_rate))
        rch = flopy.mf6.ModflowGwfrch(gwf, stress_period_data=spd)
    else:
        rch = flopy.mf6.ModflowGwfrcha(gwf, recharge=rch_rate)
    return rch


def build_model(idx, ws, gridgen):
    return build_mf6(idx, ws, gridgen), None


# build MODFLOW 6 files
def build_mf6(idx, ws, gridgen):
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="DAYS",
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="simple",
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        print_input=True,
        save_flows=True,
    )

    if "disv" in name:
        dis = build_disv(ws, gwf, gridgen)
    elif "disu" in name:
        dis = build_disu(ws, gwf, gridgen)
    else:
        dis = build_dis(gwf)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(
        gwf,
        strt=top,
    )

    if name.endswith("dis"):
        k11 = hk
        k33 = vk
        icelltype = [0, 0, 0]
    else:
        k11 = build_3d_array(gwf.modelgrid, hk)
        k33 = build_3d_array(gwf.modelgrid, vk)
        icelltype = build_3d_array(gwf.modelgrid, [0, 0, 0], dtype=int)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=icelltype,
        k=k11,
        k33=k33,
    )

    # canal chd
    if name.startswith("ps1"):
        canal_chd = flopy.mf6.ModflowGwfchd(
            gwf,
            filename=f"{name}.canal.chd",
            pname="canal",
            stress_period_data=build_chd_data(
                gwf.modelgrid,
                canal_coordinates,
                canal_head,
            ),
        )

    if name.startswith("ps1") or name.startswith("ps2a"):
        river_chd = flopy.mf6.ModflowGwfchd(
            gwf,
            filename=f"{name}.river.chd",
            pname="river",
            stress_period_data=build_chd_data(
                gwf.modelgrid,
                river_coordinates,
                river_head,
            ),
        )
    elif name.startswith("ps2b"):
        river_riv = flopy.mf6.ModflowGwfriv(
            gwf,
            pname="river",
            stress_period_data=build_riv_data(
                gwf.modelgrid,
            ),
        )

    if name.startswith("ps2"):
        rch = build_rch_package(gwf, name)

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )
    return sim


def eval_head(sim):
    name = ex[sim.idxsim]
    ws = pl.Path(sim.simpath)

    print(f"evaluating {name} heads...")

    if name.startswith("ps1"):
        row_values = np.array(
            [
                330.0,
                329.32282902,
                328.69917882,
                328.12080967,
                327.58024448,
                327.07064659,
                326.58570186,
                326.11951283,
                325.66649864,
                325.22130154,
                324.77869596,
                324.33349909,
                323.88048514,
                323.41429613,
                322.92935186,
                322.41975442,
                321.87918959,
                321.30082044,
                320.67717093,
                320.0,
            ]
        )
    elif name.startswith("ps2a"):
        row_values = np.array(
            [
                338.7727826,
                338.7030565,
                338.56285698,
                338.35067753,
                338.06422624,
                337.7003874,
                337.25516901,
                336.72363512,
                336.09982217,
                335.37663815,
                334.54574351,
                333.5974106,
                332.52035785,
                331.30155897,
                329.92602474,
                328.37654948,
                326.63341884,
                324.67407807,
                322.47275007,
                320.0,
            ]
        )
    elif name.startswith("ps2b"):
        row_values = np.array(
            [
                339.023,
                338.953,
                338.813,
                338.601,
                338.314,
                337.950,
                337.505,
                336.974,
                336.350,
                335.627,
                334.796,
                333.847,
                332.770,
                331.552,
                330.176,
                328.627,
                326.883,
                324.924,
                322.723,
                320.250,
            ]
        )
    else:
        row_values = None
    if row_values is None:
        answer = None
    else:
        answer = np.zeros(shape2d)
        for i in range(nrow):
            answer[i, :] = row_values[:]

    sim = flopy.mf6.MFSimulation.load(sim_name=name, sim_ws=ws)
    gwf = sim.get_model()

    head = gwf.output.head().get_data().flatten().reshape(shape3d)
    # if name.startswith("ps2"):
    #     v = head[0, 0, :]
    if answer is not None:
        assert np.allclose(
            head[0], answer
        ), "head data for first layer does not match know result"

    return


@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    ws = function_tmpdir
    test = TestFramework()
    test.build(lambda i, w: build_model(i, w, targets.gridgen), idx, ws)
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_head,
            cmp_verbose=False,
            idxsim=idx,
        ),
        ws,
    )
