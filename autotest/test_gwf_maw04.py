import os
from types import SimpleNamespace

import flopy
import numpy as np
from modflow_devtools.case import Case
from pytest_cases import parametrize

# temporal discretization
nper = 2
perlen = [0.0, 365.0]
nstp = [1, 25]
tsmult = [1.0, 1.1]
steady = [True, False]

# spatial discretization
nlay, nrow, ncol = 2, 101, 101
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol

xlen = 1000.0
common_ratio = 1.01
nhalf = int(0.5 * ncol) + 1
first_term = 0.5 * xlen / ((1 - common_ratio**nhalf) / (1 - common_ratio))

delr = np.zeros((ncol), dtype=float)
for n in range(nhalf):
    if n == 0:
        v = first_term
    else:
        v = first_term * common_ratio**n
    delr[nhalf + n - 1] = v
delr[: nhalf - 1] = delr[-1 : nhalf - 1 : -1]

# add error to edge cells
err = xlen - delr.sum()
delr[0] += 0.5 * err
delr[-1] += 0.5 * err

top = 0.0
botm = [-175, -350.0]
strt = 0.0

# hydraulic data
hk = 1.0
ss = 1e-5
confined = 0

chd_spd = []
chd5_spd = []
for i in range(nrow):
    if i == 0 or i == ncol - 1:
        for j in range(ncol):
            chd_spd.append([(0, i, j), strt])
            chd5_spd.append([0, i, j, strt, strt])
    else:
        chd_spd.append([(0, i, 0), strt])
        chd_spd.append([(0, i, ncol - 1), strt])
        chd5_spd.append([0, i, 0, strt, strt])
        chd5_spd.append([0, i, ncol - 1, strt, strt])

# maw data
radius0 = np.sqrt(delr[nhalf] * delr[nhalf] / (8.0 * np.pi))
radius = 0.25
sradius0 = radius + 0.1
wellq = -100.0
skin_mult = {"a": 0.1, "b": 10.0, "c": 1.0, "d": 0.0, "e": -1.0, "f": 100.0}
condeqn = {
    "a": "CUMULATIVE",
    "b": "SKIN",
    "c": "SKIN",
    "d": "SKIN",
    "e": "SPECIFIED",
    "f": "CUMULATIVE",
}
sradius = {
    "a": sradius0,
    "b": sradius0,
    "c": sradius0,
    "d": sradius0,
    "e": sradius0,
    "f": radius0 * 1.5,
}
hclose, rclose = 1e-9, 1e-6


def well4(label):
    hks = hk * skin_mult[label]
    packagedata = [[0, radius, botm[-1], strt, condeqn[label], 2]]
    connectiondata = [
        [0, 0, (0, nhalf, nhalf), top, botm[0], hks, sradius[label]],
        [0, 1, (1, nhalf, nhalf), botm[0], botm[1], hks, sradius[label]],
    ]
    perioddata = {1: [[0, "RATE", wellq]]}
    return SimpleNamespace(
        print_input=True,
        no_well_storage=True,
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=perioddata,
    )


case = Case(
    name="maw_iss305",
    nlay=nlay,
    nrow=nrow,
    ncol=ncol,
    nper=nper,
    delr=delr,
    perlen=perlen,
    nstp=nstp,
    tsmult=tsmult,
    steady=steady,
    strt=0,
    hk=10,
    nouter=100,
    ninner=100,
    hclose=1e-9,
    rclose=1e-6,
    relax=1,
    top=top,
    botm=botm,
    confined=confined,
    ss=ss,
    chd_spd=chd_spd,
    chd5_spd=chd5_spd,
    nhalf=nhalf,
    radius=radius,
    wellq=wellq,
    compare=False,
)
cases = [case.copy_update(name=case.name + "a", well=well4("a"),)] + [
    case.copy_update(name=case.name + label, well=well4(label), xfail=True)
    for label in [
        "b",
        # "c",  # todo: this one passes when it should fail
        "d",
        "e",
        "f",
    ]
]


class GwfMaw04Cases:
    @parametrize(data=cases, ids=[c.name for c in cases])
    def case_4(self, function_tmpdir, targets, data):
        name = data.name
        ws = str(function_tmpdir)

        # build MODFLOW 6 files
        sim = flopy.mf6.MFSimulation(
            sim_name=name, version="mf6", exe_name=targets["mf6"], sim_ws=ws
        )
        # create tdis package
        tdis_rc = []
        for idx in range(data.nper):
            tdis_rc.append(
                (data.perlen[idx], data.nstp[idx], data.tsmult[idx])
            )
        tdis = flopy.mf6.ModflowTdis(
            sim, time_units="DAYS", nper=data.nper, perioddata=tdis_rc
        )

        # create iterative model solution
        ims = flopy.mf6.ModflowIms(
            sim,
            inner_dvclose=data.hclose,
            rcloserecord=data.rclose,
            outer_dvclose=data.hclose,
        )

        # create gwf model
        gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)

        # discretization
        dis = flopy.mf6.ModflowGwfdis(
            gwf,
            nlay=data.nlay,
            nrow=data.nrow,
            ncol=data.ncol,
            delr=data.delr,
            delc=data.delr,
            top=data.top,
            botm=data.botm,
        )
        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=data.strt)

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf, save_flows=False, icelltype=data.confined, k=data.hk
        )
        # storage
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=False,
            iconvert=data.confined,
            ss=data.ss,
            steady_state={0: True},
            transient={1: True},
        )
        # constant head
        chd = flopy.mf6.ModflowGwfchd(
            gwf, stress_period_data=data.chd_spd, save_flows=False
        )
        # multi-aquifer well
        maw = flopy.mf6.ModflowGwfmaw(
            gwf,
            print_input=data.well.print_input,
            no_well_storage=data.well.no_well_storage,
            packagedata=data.well.packagedata,
            connectiondata=data.well.connectiondata,
            perioddata=data.well.perioddata,
        )
        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord=f"{name}.cbc",
            head_filerecord=f"{name}.hds",
            saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        )
        # build MODFLOW-2005 files
        if data.xfail:
            mc = None
        else:
            cmppth = "mf2005"
            ws = os.path.join(str(function_tmpdir), cmppth)
            mc = flopy.modflow.Modflow(name, model_ws=ws, version=cmppth)
            dis = flopy.modflow.ModflowDis(
                mc,
                nlay=data.nlay,
                nrow=data.nrow,
                ncol=data.ncol,
                nper=data.nper,
                perlen=data.perlen,
                nstp=data.nstp,
                tsmult=data.tsmult,
                steady=data.steady,
                delr=data.delr,
                delc=data.delr,
                top=data.top,
                botm=data.botm,
            )
            bas = flopy.modflow.ModflowBas(mc, strt=data.strt)
            lpf = flopy.modflow.ModflowLpf(
                mc,
                laytyp=data.confined,
                hk=data.hk,
                vka=data.hk,
                ss=data.ss,
                sy=0,
            )
            chd = flopy.modflow.ModflowChd(
                mc, stress_period_data=data.chd5_spd
            )
            # mnw2
            # empty mnw2 file to create recarrays
            mnw2 = flopy.modflow.ModflowMnw2(mc)
            node_data = mnw2.get_empty_node_data(2)
            node_data["ztop"] = np.array([data.top, data.botm[0]])
            node_data["zbotm"] = np.array([data.botm[0], data.botm[1]])
            node_data["i"] = np.array([data.nhalf, data.nhalf])
            node_data["j"] = np.array([data.nhalf, data.nhalf])
            node_data["wellid"] = np.array(["well1", "well1"])
            node_data["losstype"] = np.array(["skin", "skin"])
            node_data["rw"] = np.array([data.radius, data.radius])
            node_data["rskin"] = np.array(
                [sradius[name[-1]], sradius[name[-1]]]
            )
            hks = hk * skin_mult[name[-1]]
            node_data["kskin"] = np.array([hks, hks])
            dtype = [("wellid", np.unicode_, 20), ("qdes", "<f8")]
            spd0 = np.zeros(1, dtype=dtype)
            spd0["wellid"] = "well1"
            spd1 = np.zeros(1, dtype=dtype)
            spd1["wellid"] = "well1"
            spd1["qdes"] = data.wellq
            spd = {0: spd0, 1: spd1}
            mnw2 = flopy.modflow.ModflowMnw2(
                mc,
                mnwmax=1,
                node_data=node_data,
                stress_period_data=spd,
                itmp=[1, 1],
                mnwprnt=2,
            )
            oc = flopy.modflow.ModflowOc(
                mc,
                stress_period_data=None,
                save_every=1,
                save_types=["save head", "save budget"],
            )
            pcg = flopy.modflow.ModflowPcg(
                mc, hclose=data.hclose, rclose=data.rclose
            )

        return data, sim, None, None
