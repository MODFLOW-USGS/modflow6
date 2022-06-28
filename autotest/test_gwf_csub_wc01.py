import os
import sys

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

ex = ["csub_wc01a", "csub_wc02b"]
exdirs = [os.path.join("temp", s) for s in ex]

ddir = "data"
cmppth = "mf6"

dtol = 1e-3
budtol = 1e-2

paktest = "csub"

isnewton = [None, "NEWTON"]

# set travis to True when version 1.13.0 is released
continuous_integration = [True for s in ex]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
pth = os.path.join(ddir, "ibc01_ibound.ref")
ib0 = np.genfromtxt(pth)

# temporal discretization
nper = 3
perlen = [1.0, 21915.0, 21915.0]
nstp = [1, 60, 60]
tsmult = [1.0, 1.0, 1.0]
steady = [True, False, False]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# spatial discretization
nlay, nrow, ncol = 4, ib0.shape[0], ib0.shape[1]
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
nactive = np.count_nonzero(ib0) * nlay

print(nlay, nrow, ncol)

delr, delc = 2000.0, 2000.0
top = 150.0
botm = [50.0, -100.0, -150.0, -350.0]
strt = 100.0

# create ibound/idomain
ib = []
for k in range(nlay):
    ib.append(ib0.astype(int).copy())
ib = np.array(ib)
print(ib[0])

hnoflo = 1e30
hdry = -1e30

# upw data
laytyp = [1, 0, 0, 0]
hk = [4.0, 4.0, 1e-2, 4.0]
sy = [0.0, 0.0, 0.0, 0.0]
# sy = [0.3, 0., 0., 0.]

# build well stress period data
wnlays = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3]
wnrows = [0, 1, 1, 1, 2, 3, 4, 4, 5, 6, 13, 13, 15, 15, 16, 17, 17, 18, 8, 11]
wncols = [7, 4, 7, 11, 3, 11, 2, 12, 13, 1, 1, 13, 2, 12, 12, 3, 11, 6, 9, 6]
wrates0 = [2.2e3 for n in range(18)] + [0.0, 0.0]
wrates1 = [2.2e3 for n in range(18)] + [-7.2e03, -7.2e03]

w0 = []
w1 = []
ws0 = []
ws1 = []
for idx, (k, i, j) in enumerate(zip(wnlays, wnrows, wncols)):
    if ib0[i, j] < 1:
        continue
    w0.append((k, i, j, wrates0[idx]))
    w1.append((k, i, j, wrates1[idx]))
    ws0.append(((k, i, j), wrates0[idx]))
    ws1.append(((k, i, j), wrates1[idx]))
wd = {0: w0, 1: w1, 2: w0}
wd6 = {0: ws0, 1: ws1, 2: ws0}
print(wd6)

# build chd stress period data
chead = 100.0
chd1 = []
chd6 = []
for k in range(nlay):
    for j in [7, 8]:
        chd1.append((k, 19, j, chead, chead))
        chd6.append(((k, 19, j), chead))
cd = {0: chd1}
cd6 = {0: chd6}

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-6, 0.01, 0.97
fluxtol = nactive * rclose

# subwt data
cc = 0.0
cr = 0.0
void = 0.82
theta = void / (1.0 + void)
kv = 999.0
sgm = 1.7
sgs = 2.0
ini_stress = 15.0
delay_flag = 0
thick = [45.0, 70.0, 50.0, 90.0]

zthick = [
    top - botm[0],
    botm[0] - botm[1],
    botm[1] - botm[2],
    botm[2] - botm[3],
]

# beta = 0.
beta = 4.65120000e-10
gammaw = 9806.65000000
sw = beta * gammaw * theta
ss = 0.0
# ss = [sw for k in range(nlay)]

swt6 = []
csubno = 0
for k in range(nlay):
    for i in range(nrow):
        for j in range(ncol):
            iactive = 0
            if ib0[i, j] > 0:
                iactive = 1
            if i == 19 and (j == 7 or j == 8):
                iactive = 0
            if iactive > 0:
                tag = f"{k + 1:02d}_{i + 1:02d}_{j + 1:02d}"
                d = [
                    csubno,
                    (k, i, j),
                    "nodelay",
                    ini_stress,
                    thick[k],
                    1.0,
                    cc,
                    cr,
                    theta,
                    kv,
                    999.0,
                    tag,
                ]
                swt6.append(d)
                csubno += 1

ds16 = [
    0,
    0,
    0,
    2052,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
]
ds17 = [
    0,
    10000,
    0,
    10000,
    0,
    0,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
]


def build_model(idx, dir):
    sim = build_mf6(idx, dir)

    # build mf6 with interbeds
    wsc = os.path.join(dir, "mf6")
    mc = build_mf6(idx, wsc, interbed=True)

    return sim, mc


# build MODFLOW 6 files
def build_mf6(idx, ws, interbed=False):

    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=name, save_flows=True, newtonoptions=isnewton[idx]
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=ib,
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
        steady_state={0: True},
        transient={1: True},
    )

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
        gwf, maxbound=len(chd6), stress_period_data=cd6, save_flows=False
    )

    # wel files
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        print_input=True,
        print_flows=True,
        maxbound=len(ws1),
        stress_period_data=wd6,
        save_flows=False,
    )

    # csub files
    if interbed:
        sswt6 = swt6
        ninterbeds = len(sswt6)
    else:
        sswt6 = None
        ninterbeds = 0
    opth = f"{name}.csub.obs"
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        # interbed_stress_offset=True,
        boundnames=True,
        compression_indices=True,
        ninterbeds=ninterbeds,
        sgs=sgs,
        sgm=sgm,
        beta=beta,
        gammaw=gammaw,
        cg_ske_cr=0.0,
        cg_theta=theta,
        packagedata=sswt6,
    )
    orecarray = {}
    orecarray["csub_obs.csv"] = [
        ("wc01", "wcomp-csub-cell", (1, 5, 8)),
        ("wc02", "wcomp-csub-cell", (3, 6, 11)),
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
        printrecord=[("HEAD", "LAST"), ("BUDGET", "ALL")],
    )
    return sim


def eval_wcomp(sim):
    print("evaluating compaction...")

    # MODFLOW 6 without interbeds water compressibility
    fpth = os.path.join(sim.simpath, "csub_obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    # MODFLOW 6 with interbeds water compressibility
    fpth = os.path.join(sim.simpath, cmppth, "csub_obs.csv")
    try:
        tci = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    diffmax = 0.0
    tagmax = None
    for tag in tc.dtype.names[1:]:
        diff = tc[tag] - tci[tag]
        diffmaxt = np.abs(diff).max()
        if diffmaxt > diffmax:
            diffmax = diffmaxt
            tagmax = tag

    msg = (
        "maximum absolute water compressibility difference "
        + f"({diffmax}) in tag: {tagmax}"
    )

    # write summary
    fpth = os.path.join(
        sim.simpath, f"{os.path.basename(sim.name)}.wcomp.cmp.out"
    )
    f = open(fpth, "w")
    line = f"{'TOTIM':>15s}"
    for tag in tc.dtype.names[1:]:
        line += f" {f'{tag}_SK':>15s}"
        line += f" {f'{tag}_SKIB':>15s}"
        line += f" {f'{tag}_DIFF':>15s}"
    f.write(line + "\n")
    for i in range(diff.shape[0]):
        line = f"{tc['time'][i]:15g}"
        for tag in tc.dtype.names[1:]:
            line += f" {tc[tag][i]:15g}"
            line += f" {tci[tag][i]:15g}"
            line += f" {tc[tag][i] - tci[tag][i]:15g}"
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


# compare cbc and lst budgets
def cbc_compare(sim):
    print("evaluating cbc and budget...")
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
        msg += f"exceeds {dtol}"
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)


# - No need to change any code below


@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # determine if running on CI infrastructure
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    if is_CI and not continuous_integration[idx]:
        return
    test.run_mf6(Simulation(dir, exe_dict=r_exe, exfunc=eval_wcomp))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for dir in exdirs:
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exe_dict=replace_exe, exfunc=eval_wcomp)
        test.run_mf6(sim)

    return


# main
if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
