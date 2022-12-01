import os

import numpy as np
import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

import targets
from framework import running_on_CI, testing_framework
from simulation import Simulation

ex = ["zbud6_zb01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

ddir = "data"

## run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

htol = [None for idx in range(len(exdirs))]
dtol = 1e-3
budtol = 1e-2

bud_lst = [
    "STO-SS_IN",
    "STO-SS_OUT",
    "STO-SY_IN",
    "STO-SY_OUT",
    "RCHA_IN",
    "RCHA_OUT",
    "CHD_IN",
    "CHD_OUT",
    "WEL_IN",
    "WEL_OUT",
]
zone_lst = []
for name in bud_lst:
    s = name.replace("_", "-")
    zone_lst.append(s)

# static model data
# temporal discretization
nper = 31
perlen = [1.0] + [365.2500000 for i in range(nper - 1)]
nstp = [1] + [6 for i in range(nper - 1)]
tsmult = [1.0] + [1.3 for i in range(nper - 1)]
# tsmult = [1.0] + [1.0 for i in range(nper - 1)]
steady = [True] + [False for i in range(nper - 1)]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# spatial discretization data
nlay, nrow, ncol = 3, 10, 10
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 1000.0, 2000.0
tops = [0.0]
botm = [-100, -150.0, -350.0]
strt = 0.0
hnoflo = 1e30
hdry = -1e30

# calculate hk
hk1fact = 1.0 / 50.0
hk1 = np.ones((nrow, ncol), dtype=float) * 0.5 * hk1fact
hk1[0, :] = 1000.0 * hk1fact
hk1[-1, :] = 1000.0 * hk1fact
hk1[:, 0] = 1000.0 * hk1fact
hk1[:, -1] = 1000.0 * hk1fact
hk = [20.0, hk1, 5.0]

# calculate vka
vka = [1e6, 7.5e-5, 1e6]

# all cells are active and layer 1 is convertible
ib = 1
laytyp = [1, 0, 0]

# solver options
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.0
newtonoptions = "NEWTON"
imsla = "BICGSTAB"

# chd data
c = []
c6 = []
ccol = [3, 4, 5, 6]
for j in ccol:
    c.append([0, nrow - 1, j, strt, strt])
    c6.append([(0, nrow - 1, j), strt])
cd = {0: c}
cd6 = {0: c6}
maxchd = len(cd[0])

# pumping well data
wr = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3]
wc = [0, 1, 8, 9, 0, 9, 0, 9, 0, 0]
wrp = [2, 2, 3, 3]
wcp = [5, 6, 5, 6]
wq = [-14000.0, -8000.0, -5000.0, -3000.0]
d = []
d6 = []
for r, c, q in zip(wrp, wcp, wq):
    d.append([2, r, c, q])
    d6.append([(2, r, c), q])
wd = {1: d}
wd6 = {1: d6}
maxwel = len(wd[1])

# recharge data
q = 3000.0 / (delr * delc)
v = np.zeros((nrow, ncol), dtype=float)
for r, c in zip(wr, wc):
    v[r, c] = q
rech = {0: v}

# storage and compaction data
ske = [6e-4, 3e-4, 6e-4]


# variant SUB package problem 3
def build_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    top = tops[idx]
    zthick = [top - botm[0], botm[0] - botm[1], botm[1] - botm[2]]
    elevs = [top] + botm

    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=name, newtonoptions=newtonoptions, save_flows=True
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
        linear_acceleration=imsla,
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
        filename=f"{name}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename=f"{name}.ic")

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        # dev_modflowusg_upstream_weighted_saturation=True,
        icelltype=laytyp,
        k=hk,
        k33=vka,
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=ske,
        sy=0,
        storagecoefficient=None,
        steady_state={0: True},
        transient={1: True},
    )

    # recharge
    rch = flopy.mf6.ModflowGwfrcha(gwf, readasarrays=True, recharge=rech)

    # wel file
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        print_input=True,
        print_flows=True,
        maxbound=maxwel,
        stress_period_data=wd6,
        save_flows=False,
    )

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
        gwf, maxbound=maxchd, stress_period_data=cd6, save_flows=False
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

    return sim, None


def eval_zb6(sim):

    print("evaluating zonebudget...")

    # build zonebudget files
    zones = [-1000000, 1000000, 9999999]
    nzones = len(zones)
    fpth = os.path.join(sim.simpath, "zonebudget.nam")
    f = open(fpth, "w")
    f.write("BEGIN ZONEBUDGET\n")
    f.write(f"  BUD {os.path.basename(sim.name)}.cbc\n")
    f.write(f"  ZON {os.path.basename(sim.name)}.zon\n")
    f.write(f"  GRB {os.path.basename(sim.name)}.dis.grb\n")
    f.write("END ZONEBUDGET\n")
    f.close()

    fpth = os.path.join(sim.simpath, f"{os.path.basename(sim.name)}.zon")
    f = open(fpth, "w")
    f.write("BEGIN DIMENSIONS\n")
    f.write(f"  NCELLS {size3d}\n")
    f.write("END DIMENSIONS\n\n")
    f.write("BEGIN GRIDDATA\n")
    f.write("  IZONE LAYERED\n")
    for k in range(nlay):
        f.write(f"    CONSTANT {zones[k]:>10d}\n")
    f.write("END GRIDDATA\n")
    f.close()

    # run zonebudget
    zbexe = os.path.abspath(targets.target_dict["zbud6"])
    success, buff = flopy.run_model(
        zbexe,
        "zonebudget.nam",
        model_ws=sim.simpath,
        silent=False,
        report=True,
    )
    if success:
        print(f"successfully ran...{os.path.basename(zbexe)}")
        sim.success = True
    else:
        sim.success = False
        msg = f"could not run...{zbexe}"
        assert success, msg

    # read data from csv file
    fpth = os.path.join(sim.simpath, "zonebudget.csv")
    zbd = np.genfromtxt(fpth, names=True, delimiter=",", deletechars="")

    # sum the data for all zones
    nentries = int(zbd.shape[0] / 3)
    zbsum = np.zeros(nentries, dtype=zbd.dtype)
    static = ["totim", "kstp", "kper"]
    ipos = 0
    ion = 0
    for t in zbd:
        for name in zbd.dtype.names:
            if name in static:
                zbsum[name][ipos] = t[name]
            elif name == "zone":
                zbsum[name][ipos] = 0
            else:
                zbsum[name][ipos] += t[name]
        ion += 1
        if ion == 3:
            ipos += 1
            ion = 0

    # get results from listing file
    fpth = os.path.join(sim.simpath, f"{os.path.basename(sim.name)}.lst")
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]

    # get results from cbc file
    cbc_bud = ["STO-SS", "STO-SY", "RCHA", "CHD", "WEL"]
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.0
    fpth = os.path.join(sim.simpath, f"{os.path.basename(sim.name)}.cbc")
    cobj = flopy.utils.CellBudgetFile(fpth, precision="double")
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

    # compare zone budget to cbc output
    diffzb = np.zeros((nbud, len(bud_lst)), dtype=float)
    for idx, (key0, key) in enumerate(zip(zone_lst, bud_lst)):
        diffzb[:, idx] = zbsum[key0] - d[key]
    diffzbmax = np.abs(diffzb).max()
    msg += (
        f"\nmaximum absolute zonebudget-cell by cell difference ({diffzbmax}) "
    )

    # write summary
    fpth = os.path.join(
        sim.simpath, f"{os.path.basename(sim.name)}.zbud.cmp.out"
    )
    f = open(fpth, "w")
    for i in range(diff.shape[0]):
        if i == 0:
            line = f"{'TIME':>10s}"
            for idx, key in enumerate(bud_lst):
                line += f"{key + '_ZBUD':>25s}"
                line += f"{key + '_CBC':>25s}"
                line += f"{key + '_DIF':>25s}"
            f.write(line + "\n")
        line = f"{d['totim'][i]:10g}"
        for idx, (key0, key) in enumerate(zip(zone_lst, bud_lst)):
            line += f"{zbsum[key0][i]:25g}"
            line += f"{d[key][i]:25g}"
            line += f"{diffzb[i, idx]:25g}"
        f.write(line + "\n")
    f.close()

    if diffmax > budtol or diffzbmax > budtol:
        sim.success = False
        msg += f"\n...exceeds {budtol}"
        assert diffmax < budtol and diffzbmax < budtol, msg
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

    # determine if running on CI infrastructure
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models_legacy(build_model, idx, dir)

    # run the test model
    if is_CI and not continuous_integration[idx]:
        return
    test.run_mf6(
        Simulation(
            dir, exfunc=eval_zb6, exe_dict=r_exe, htol=htol[idx], idxsim=idx
        )
    )


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models_legacy(build_model, idx, dir)
        sim = Simulation(
            dir,
            exfunc=eval_zb6,
            exe_dict=replace_exe,
            htol=htol[idx],
            idxsim=idx,
        )
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
