import flopy
import numpy as np
import pytest
from flopy.utils.gridgen import Gridgen

from framework import TestFramework
from simulation import TestSimulation

ex = ["csub_dis", "csub_disv", "csub_disu", "csub_disu01", "csub_disu02"]
# ex = ["csub_dis"]
ex_dict = {name: None for name in ex}
ex_dict["csub_disu01"] = 0
ex_dict["csub_disu02"] = 2

paktest = "csub"

# temporal discretization
nper = 2
perlen = [1.0, 100.0]
nstp = [1, 10]
tsmult = [1.0] * nper
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# base spatial discretization
nlay, nrow, ncol = 3, 9, 9
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol

delr = delc = 1000.0
top = 0.0
bot = -100.0
dz = (top - bot) / nlay
botm = [top - k * dz for k in range(1, nlay + 1)]
z_node = [z + 0.5 * dz for z in botm]

hk = [1.0, 0.001, 1.0]
sy = [0.25, 0.45, 0.25]
ss = [5e-5, 5e-4, 5e-5]

well_coordinates = (
    np.array([(4.25, 4.25), (4.25, 4.75), (4.75, 4.75), (4.75, 4.25)]) * delr
)
wellq = -1000.0

nouter, ninner = 100, 300
dvclose, rclose, relax = 1e-6, 0.01, 0.97

# subwt data
cc = 0.25
cr = 0.25
void = 0.82
theta = void / (1.0 + void)
kv = 999.0
sgm = 1.7
sgs = 2.0

beta = 0.0
# beta = 4.65120000e-10
gammaw = 9806.65000000


def get_ske():
    # gsb = np.zeros((nlay), dtype=float)
    # ub = np.zeros((nlay), dtype=float)
    # esb = np.zeros((nlay), dtype=float)
    # ske = np.zeros((nlay), dtype=float)
    #
    # # calculate incremental geostatic stress and hydrostatic stress
    # for k in range(nlay):
    #     zt = zelv[k]
    #     zb = zelv[k + 1]
    #     b = zthick[k]
    #     if strt >= zt:
    #         gs = b * sgs
    #     elif strt < zb:
    #         gs = b * sgm
    #     else:
    #         gs = (zt - strt) * sgm + (strt - zb) * sgs
    #     gsb[k] = gs
    #     ub[k] = strt - zb
    #
    # # calculate geostatic and effective stress at the bottom of the layer
    # gsb = np.cumsum(gsb)
    # esb = gsb - ub
    #
    # # calculate ske
    # fact = 0.4342942
    # ggammaw = 1.0  # gammaw * (60. * 60. * 24.)**2.
    # for k in range(nlay):
    #     zt = zelv[k]
    #     zb = zelv[k + 1]
    #     if strt >= zt:
    #         z = 0.5 * (zt + zb)
    #     elif strt < zb:
    #         z = zb
    #     else:
    #         z = 0.5 * (strt + zb)
    #     es = esb[k] - (z - zb) * (sgs - 1.0)
    #     ske[k] = fact * cr * ggammaw / (es * (1 + void))
    #
    # return ske.tolist()
    return


def get_interbed(modelgrid):
    grid_type = modelgrid.grid_type
    ia = []
    x0, x1, y0, y1 = modelgrid.extent
    for k in range(1, nlay, 1):
        cellid = modelgrid.intersect(x0 + 0.1, y1 - 0.1, z=z_node[k])
        ia.append(get_node_number(modelgrid, cellid))

    package_data = []
    ifno = 0
    ini_stress = 0.0

    nodes = [node for node in range(ia[0], ia[1])]
    if grid_type == "structured":
        cellids = modelgrid.get_lrc(nodes)
    elif grid_type == "vertex":
        cellids = modelgrid.get_lni(nodes)
    else:
        cellids = [(node,) for node in nodes]

    for cellid in cellids:
        rnb = 1.0
        vk = 999.0
        package_data.append(
            (
                ifno,
                cellid,  # will need to be detuplaized with *cellid - does not work for dis
                "nodelay",
                ini_stress,
                modelgrid.cell_thickness[cellid],
                rnb,
                ss[1],
                ss[1] * 1000.0,
                theta,
                vk,
                top,
            )
        )
        ifno += 1
    return package_data


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


def build_disu(ws, gwf, refinement_layer, gridgen):
    temp_gwf = build_temp_gwf(ws)
    dis = build_dis(temp_gwf)
    g = Gridgen(
        temp_gwf.modelgrid,
        model_ws=get_gridgen_ws(ws),
        exe_name=gridgen,
    )
    if refinement_layer is not None:
        x0, x1, y0, y1 = temp_gwf.modelgrid.extent
        polys = [[[(x0, y0), (x1, y0), (x1, y1), (x0, y1), (x0, y0)]]]
        g.add_refinement_features(
            polys,
            "polygon",
            1,
            layers=[refinement_layer],
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


def build_well_data(modelgrid):
    well_spd = []
    for x, y in well_coordinates:
        cellid = modelgrid.intersect(x, y, z=z_node[-1])
        if isinstance(cellid, tuple):
            well_spd.append((*cellid, wellq))
        else:
            well_spd.append((cellid, wellq))
    return {1: well_spd}


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
        nper=nper,
        perioddata=tdis_rc,
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=dvclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=dvclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
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
        dis = build_disu(ws, gwf, ex_dict[name], gridgen)
    else:
        dis = build_dis(gwf)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(
        gwf,
        strt=top,
    )

    k11 = build_3d_array(gwf.modelgrid, hk)
    icelltype = build_3d_array(gwf.modelgrid, [1, 0, 0], dtype=int)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=icelltype,
        k=k11,
        k33=k11,
    )

    # storage
    sy_arr = build_3d_array(gwf.modelgrid, sy)
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        iconvert=icelltype,
        ss=0.0,
        sy=sy_arr,
        transient={0: True},
    )

    # wel files
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        stress_period_data=build_well_data(gwf.modelgrid),
        save_flows=False,
    )

    # csub files
    cg_ske_cr = build_3d_array(gwf.modelgrid, ss)
    packagedata = get_interbed(gwf.modelgrid)

    # if interbed:
    #     cg_ske_cr[2] = 0
    # opth = f"{name}.csub.obs"
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        zdisplacement_filerecord=f"{name}.csub.zdis.bin",
        # compression_indices=ci,
        ninterbeds=len(packagedata),
        sgs=sgs,
        sgm=sgm,
        beta=beta,
        gammaw=gammaw,
        cg_ske_cr=cg_ske_cr,
        cg_theta=theta,
        packagedata=packagedata,
    )
    # orecarray = {}
    # orecarray["csub_obs.csv"] = [
    #     ("wc01", "compaction-cell", (1, 5, 8)),
    #     ("wc02", "compaction-cell", (3, 6, 11)),
    # ]
    # csub_obs_package = csub.obs.initialize(
    #     filename=opth, digits=10, print_input=True, continuous=orecarray
    # )

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


def eval_comp(sim):
    print("evaluating compaction...")

    # # MODFLOW 6 without interbeds
    # fpth = os.path.join(sim.simpath, "csub_obs.csv")
    # try:
    #     tc = np.genfromtxt(fpth, names=True, delimiter=",")
    # except:
    #     assert False, f'could not load data from "{fpth}"'

    # # MODFLOW 6 with interbeds
    # fpth = os.path.join(sim.simpath, cmppth, "csub_obs.csv")
    # try:
    #     tci = np.genfromtxt(fpth, names=True, delimiter=",")
    # except:
    #     assert False, f'could not load data from "{fpth}"'

    # diffmax = 0.0
    # tagmax = None
    # for tag in tc.dtype.names[1:]:
    #     diff = tc[tag] - tci[tag]
    #     diffmaxt = np.abs(diff).max()
    #     if diffmaxt > diffmax:
    #         diffmax = diffmaxt
    #         tagmax = tag

    # msg = "maximum compaction difference " + f"({diffmax}) in tag: {tagmax}"

    # # write summary
    # fpth = os.path.join(
    #     sim.simpath, f"{os.path.basename(sim.name)}.comp.cmp.out"
    # )
    # f = open(fpth, "w")
    # line = f"{'TOTIM':>15s}"
    # for tag in tc.dtype.names[1:]:
    #     line += f" {f'{tag}_SK':>15s}"
    #     line += f" {f'{tag}_SKIB':>15s}"
    #     line += f" {f'{tag}_DIFF':>15s}"
    # f.write(line + "\n")
    # for i in range(diff.shape[0]):
    #     line = f"{tc['time'][i]:15g}"
    #     for tag in tc.dtype.names[1:]:
    #         line += f" {tc[tag][i]:15g}"
    #         line += f" {tci[tag][i]:15g}"
    #         line += f" {tc[tag][i] - tci[tag][i]:15g}"
    #     f.write(line + "\n")
    # f.close()

    # if diffmax > dtol:
    #     sim.success = False
    #     msg += f"exceeds {dtol}"
    #     assert diffmax < dtol, msg
    # else:
    #     sim.success = True
    #     print("    " + msg)
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
            exfunc=eval_comp,
            cmp_verbose=False,
        ),
        ws,
    )
