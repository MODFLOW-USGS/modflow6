import os

import numpy as np
import pytest

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import running_on_CI, testing_framework
from simulation import Simulation

paktest = "csub"
budtol = 1e-2

ex = ["csub_sub01a", "csub_sub01b"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"

compression_indices = [None, True]

ndcell = [19] * len(ex)

# run all examples on Travis
# continuous_integration = [True for idx in range(len(exdirs))]
# the delay bed problems only run on the development version of MODFLOW-2005
# set travis to True when version 1.13.0 is released
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
# spatial discretization
nlay, nrow, ncol = 1, 1, 3
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 1.0, 1.0
top = 0.0
botm = [-100.0]

# temporal discretization
nper = 1
perlen = [1000.0 for i in range(nper)]
nstp = [100 for i in range(nper)]
tsmult = [1.05 for i in range(nper)]
steady = [False for i in range(nper)]

strt = 0.0
strt6 = 1.0
hnoflo = 1e30
hdry = -1e30
hk = 1e6
laytyp = [0]
S = 1e-4
sy = 0.0

nouter, ninner = 1000, 300
hclose, rclose, relax = 1e-6, 1e-6, 0.97

tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

ib = 1

c = []
c6 = []
for j in range(0, ncol, 2):
    c.append([0, 0, j, strt, strt])
    c6.append([(0, 0, j), strt])
cd = {0: c}
cd6 = {0: c6}

# sub data
ndb = 1
nndb = 0
cc = 100.0
cr = 1.0
void = 0.82
theta = void / (1.0 + void)
kv = 0.025
sgm = 0.0
sgs = 0.0
ini_stress = 1.0
thick = [1.0]
sfe = cr * thick[0]
sfv = cc * thick[0]
lnd = [0]
ldnd = [0]
dp = [[kv, cr, cc]]
ss = S / (100.0 - thick[0])

ds15 = [0, 0, 0, 2052, 0, 0, 0, 0, 0, 0, 0, 0]
ds16 = [0, 0, 0, 100, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1]


def get_model(idx, ws):
    name = ex[idx]

    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create iterative model solution
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        filename=f"{name}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename=f"{name}.ic")

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=False, icelltype=laytyp, k=hk, k33=hk
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=0.0,
        sy=sy,
        storagecoefficient=True,
        transient={0: True},
    )

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
        gwf, maxbound=len(c6), stress_period_data=cd6, save_flows=False
    )

    # csub files
    ci = compression_indices[idx]
    if ci is None:
        sub6 = [
            [
                0,
                (0, 0, 1),
                "delay",
                ini_stress,
                thick[0],
                1.0,
                cc,
                cr,
                theta,
                kv,
                ini_stress,
            ]
        ]
    else:
        sub6 = [
            [
                0,
                (0, 0, 1),
                "delay",
                ini_stress,
                thick[0],
                1.0,
                230.258658761733000,
                2.302586587617330,
                theta,
                kv,
                ini_stress,
            ]
        ]

    opth = f"{name}.csub.obs"
    cnvgpth = f"{name}.csub.cnvg.csv"
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        head_based=True,
        compression_indices=ci,
        print_input=True,
        save_flows=True,
        package_convergence_filerecord=cnvgpth,
        effective_stress_lag=True,
        ndelaycells=ndcell[idx],
        ninterbeds=1,
        beta=0.0,
        cg_ske_cr=ss,
        packagedata=sub6,
    )
    orecarray = {}
    orecarray["csub_obs.csv"] = [
        ("tcomp", "compaction-cell", (0, 0, 1)),
        ("sk", "sk", (0, 0, 1)),
    ]
    csub_obs_package = csub.obs.initialize(
        filename=opth, digits=10, print_input=True, continuous=orecarray
    )

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


def build_model(idx, dir):

    # build MODFLOW 6 files
    ws = dir
    sim = get_model(idx, ws)

    # build MODFLOW-2005 files
    ws = os.path.join(dir, "mf6-regression")
    mc = get_model(idx, ws)

    return sim, mc


def eval_sub(sim):
    print("evaluating subsidence...")

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, "csub_obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    # comparison total compaction results
    fpth = os.path.join(sim.simpath, "mf6-regression", "csub_obs.csv")
    try:
        tc0 = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    # calculate maximum absolute error
    diff = tc["TCOMP"] - tc0["TCOMP"]
    diffmax = np.abs(diff).max()
    dtol = 1e-6
    msg = f"maximum absolute total-compaction difference ({diffmax}) "

    # write summary
    fpth = os.path.join(
        sim.simpath, f"{os.path.basename(sim.name)}.comp.cmp.out"
    )
    f = open(fpth, "w")
    line = f"{'TOTIM':>15s}"
    line += f" {'CSUB':>15s}"
    line += f" {'MF':>15s}"
    line += f" {'DIFF':>15s}"
    f.write(line + "\n")
    for i in range(diff.shape[0]):
        line = f"{tc0['time'][i]:15g}"
        line += f" {tc['TCOMP'][i]:15g}"
        line += f" {tc0['TCOMP'][i]:15g}"
        line += f" {diff[i]:15g}"
        f.write(line + "\n")
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += f"exceeds {dtol}"
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    # compare budgets
    cbc_compare(sim)

    return


# compare cbc and lst budgets
def cbc_compare(sim):
    # open cbc file
    fpth = os.path.join(sim.simpath, f"{os.path.basename(sim.name)}.cbc")
    cobj = flopy.utils.CellBudgetFile(fpth, precision="double")

    # build list of cbc data to retrieve
    avail = cobj.get_unique_record_names()
    cbc_bud = []
    bud_lst = []
    for t in avail:
        if isinstance(t, bytes):
            t = t.decode()
        t = t.strip()
        if paktest in t.lower():
            cbc_bud.append(t)
            bud_lst.append(f"{t}_IN")
            bud_lst.append(f"{t}_OUT")

    # get results from listing file
    fpth = os.path.join(sim.simpath, f"{os.path.basename(sim.name)}.lst")
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.0

    # get data from cbc dile
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.0
            qout = 0.0
            v = cobj.get_data(kstpkper=k, text=text)[0]
            if isinstance(v, np.recarray):
                vt = np.zeros(size3d, dtype=float)
                for jdx, node in enumerate(v["node"]):
                    vt[node - 1] += v["q"][jdx]
                v = vt.reshape(shape3d)
            for kk in range(v.shape[0]):
                for ii in range(v.shape[1]):
                    for jj in range(v.shape[2]):
                        vv = v[kk, ii, jj]
                        if vv < 0.0:
                            qout -= vv
                        else:
                            qin += vv
            d["totim"][idx] = t
            d["time_step"][idx] = k[0]
            d["stress_period"] = k[1]
            key = f"{text}_IN"
            d[key][idx] = qin
            key = f"{text}_OUT"
            d[key][idx] = qout

    diff = np.zeros((nbud, len(bud_lst)), dtype=float)
    for idx, key in enumerate(bud_lst):
        diff[:, idx] = d0[key] - d[key]
    diffmax = np.abs(diff).max()
    msg = f"maximum absolute total-budget difference ({diffmax}) "

    # write summary
    fpth = os.path.join(
        sim.simpath, f"{os.path.basename(sim.name)}.bud.cmp.out"
    )
    f = open(fpth, "w")
    for i in range(diff.shape[0]):
        if i == 0:
            line = f"{'TIME':>10s}"
            for idx, key in enumerate(bud_lst):
                line += f"{key + '_LST':>25s}"
                line += f"{key + '_CBC':>25s}"
                line += f"{key + '_DIF':>25s}"
            f.write(line + "\n")
        line = f"{d['totim'][i]:10g}"
        for idx, key in enumerate(bud_lst):
            line += f"{d0[key][i]:25g}"
            line += f"{d[key][i]:25g}"
            line += f"{diff[i, idx]:25g}"
        f.write(line + "\n")
    f.close()

    if diffmax > budtol:
        sim.success = False
        msg += f"diffmax {diffmax} exceeds tolerance {budtol}"
        assert diffmax < budtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, exdir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, exdir):
    # initialize testing framework
    test = testing_framework()

    # run the test model
    test.build_mf6_models(build_model, idx, exdir)

    test.run_mf6(
        Simulation(
            exdir,
            exfunc=eval_sub,
            idxsim=idx,
            mf6_regression=True,
        )
    )


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, exdir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, exdir)
        sim = Simulation(
            exdir,
            exfunc=eval_sub,
            idxsim=idx,
            mf6_regression=True,
        )
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
