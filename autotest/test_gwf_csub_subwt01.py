import os
import pytest
import sys
import numpy as np

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

from framework import testing_framework, running_on_CI
from simulation import Simulation

ex = ["csub_subwt01a", "csub_subwt01b", "csub_subwt01c", "csub_subwt01d"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
cmppth = "mf6-regression"

htol = [None for n in ex]
dtol = 1e-3
budtol = 1e-2

paktest = "csub"

ump = [None, True, None, True]
ivoid = [0, 1, 0, 1]
gs0 = [0.0, 0.0, 1700.0, 1700.0]

# set travis to True when version 1.13.0 is released
continuous_integration = [True for n in ex]

# set replace_exe to None to use default executable
replace_exe = None

# temporal discretization
nper = 3
perlen = [1.0, 21915.0, 21915.0]
nstp = [1, 60, 60]
tsmult = [1.0, 1.0, 1.0]
steady = [True, False, False]

# spatial discretization
nlay, nrow, ncol = 1, 1, 3
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol

delr, delc = 2000.0, 2000.0
top = 150.0
botm = [-350.0]
strt = 100.0

hnoflo = 1e30
hdry = -1e30

# upw data
laytyp = [1]
hk = [4.0]
sy = [0.3]

hstart = [100.0, 50.0]

chd0 = [
    (0, 0, 0, hstart[0] + 1, hstart[0] + 1),
    (0, 0, 2, hstart[0], hstart[0]),
]
chd1 = [
    (0, 0, 0, hstart[1] + 1, hstart[1] + 1),
    (0, 0, 2, hstart[1], hstart[1]),
]
cd = {0: chd0, 1: chd1, 2: chd0}

chd6_0 = [((0, 0, 0), hstart[0] + 1), ((0, 0, 2), hstart[0])]
chd6_1 = [((0, 0, 0), hstart[1] + 1), ((0, 0, 2), hstart[1])]
cd6 = {0: chd6_0, 1: chd6_1, 2: chd6_0}

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-6, 0.01, 0.97
fluxtol = rclose

tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# this used to work
# ib = np.zeros((nlay, nrow, ncol), dtype=int)
# for k in range(nlay):
ib = [1]

# subwt data
cc = 0.25
cr = 0.01
void = 0.82
theta = void / (1.0 + void)
kv = 999.0
sgm = 1.7
sgs = 2.0
ini_stress = 0.0  # 15.0
delay_flag = 0
thick = [45.0, 70.0, 50.0, 90.0]

zthick = [top - botm[0]]

beta = 0.0
# beta = 4.65120000e-10
gammaw = 9806.65000000
sw = beta * gammaw * theta
ss = [sw for k in range(nlay)]

swt6 = []
ibcno = 0
for k in range(len(thick)):
    for i in range(nrow):
        for j in range(ncol):
            iactive = 0
            if j == 1:
                iactive = 1
            if iactive > 0:
                tag = "{:02d}_{:02d}_{:02d}".format(1, i + 1, j + 1)
                d = [
                    ibcno,
                    (0, i, j),
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
                ibcno += 1


def get_model(idx, ws):
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        memory_print_option="all",
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
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
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=name, save_flows=True, print_input=True
    )

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
        filename="{}.dis".format(name),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename="{}.ic".format(name))

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
        gwf, maxbound=len(chd6_0), stress_period_data=cd6, save_flows=False
    )

    # csub files
    gg = []
    for i in range(nrow):
        for j in range(ncol):
            gg.append([(0, i, j), gs0[idx]])
    sig0 = {0: gg}
    opth = "{}.csub.obs".format(name)
    fcgstrain = "{}.csub.strain.cg.csv".format(name)
    fibstrain = "{}.csub.strain.ib.csv".format(name)
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        straincg_filerecord=fcgstrain,
        strainib_filerecord=fibstrain,
        boundnames=True,
        compression_indices=True,
        update_material_properties=ump[idx],
        effective_stress_lag=True,
        ninterbeds=len(swt6),
        sgs=sgs,
        sgm=sgm,
        beta=beta,
        gammaw=gammaw,
        cg_ske_cr=0.0,
        cg_theta=theta,
        packagedata=swt6,
        maxsig0=len(gg),
        stress_period_data=sig0,
    )
    orecarray = {}
    orecarray["csub_obs.csv"] = [
        ("w1l1", "interbed-compaction", "01_01_02"),
        ("w1l1t", "csub-cell", (0, 0, 1)),
    ]
    csub_obs_package = csub.obs.initialize(
        filename=opth, digits=10, print_input=True, continuous=orecarray
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(name),
        head_filerecord="{}.hds".format(name),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "ALL")],
    )
    return sim


def build_model(idx, dir):

    # build MODFLOW 6 files
    ws = dir
    sim = get_model(idx, ws)

    # build comparision files
    ws = os.path.join(dir, cmppth)
    mc = get_model(idx, ws)

    return sim, mc


def eval_comp(sim):

    print("evaluating compaction...")
    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, "csub_obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # Comparision total compaction results
    cpth = cmppth
    fpth = os.path.join(sim.simpath, cpth, "csub_obs.csv")
    try:
        tc0 = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # calculate maximum absolute error
    loctag = "W1L1"
    diff = tc[loctag] - tc0[loctag]
    diffmax = np.abs(diff).max()
    msg = "maximum absolute total-compaction difference ({}) ".format(diffmax)

    # write summary
    fpth = os.path.join(
        sim.simpath, "{}.comp.cmp.out".format(os.path.basename(sim.name))
    )
    f = open(fpth, "w")
    line = "{:>15s}".format("TOTIM")
    line += " {:>15s}".format("CSUB")
    line += " {:>15s}".format("MF")
    line += " {:>15s}".format("DIFF")
    f.write(line + "\n")
    for i in range(diff.shape[0]):
        line = "{:15g}".format(tc0["time"][i])
        line += " {:15g}".format(tc[loctag][i])
        line += " {:15g}".format(tc0[loctag][i])
        line += " {:15g}".format(diff[i])
        f.write(line + "\n")
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += "exceeds {}".format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    # compare budgets
    cbc_compare(sim)

    return


# compare cbc and lst budgets
def cbc_compare(sim):
    print("evaluating cbc and budget...")
    # open cbc file
    fpth = os.path.join(
        sim.simpath, "{}.cbc".format(os.path.basename(sim.name))
    )
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
            bud_lst.append("{}_IN".format(t))
            bud_lst.append("{}_OUT".format(t))

    # get results from listing file
    fpth = os.path.join(
        sim.simpath, "{}.lst".format(os.path.basename(sim.name))
    )
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
            key = "{}_IN".format(text)
            d[key][idx] = qin
            key = "{}_OUT".format(text)
            d[key][idx] = qout

    diff = np.zeros((nbud, len(bud_lst)), dtype=float)
    for idx, key in enumerate(bud_lst):
        diff[:, idx] = d0[key] - d[key]
    diffmax = np.abs(diff).max()
    msg = "maximum absolute total-budget difference ({}) ".format(diffmax)

    # write summary
    fpth = os.path.join(
        sim.simpath, "{}.bud.cmp.out".format(os.path.basename(sim.name))
    )
    f = open(fpth, "w")
    for i in range(diff.shape[0]):
        if i == 0:
            line = "{:>10s}".format("TIME")
            for idx, key in enumerate(bud_lst):
                line += "{:>25s}".format(key + "_LST")
                line += "{:>25s}".format(key + "_CBC")
                line += "{:>25s}".format(key + "_DIF")
            f.write(line + "\n")
        line = "{:10g}".format(d["totim"][i])
        for idx, key in enumerate(bud_lst):
            line += "{:25g}".format(d0[key][i])
            line += "{:25g}".format(d[key][i])
            line += "{:25g}".format(diff[i, idx])
        f.write(line + "\n")
    f.close()

    if diffmax > budtol:
        sim.success = False
        msg += "exceeds {}".format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below


@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # determine if running on Travis or GitHub actions
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
    test.run_mf6(
        Simulation(
            dir,
            exe_dict=r_exe,
            exfunc=eval_comp,
            htol=htol[idx],
            mf6_regression=True,
        )
    )


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for dir in exdirs:
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(
            dir,
            exe_dict=replace_exe,
            exfunc=eval_comp,
            htol=htol[idx],
            mf6_regression=True,
        )
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
