import os
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

ex = ["csub_sub03a", "csub_sub03b"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
cvopt = [None, None, None]
constantcv = [True, True]
ndelaybeds = [0, 2]
ndelaycells = [None, 39]

ddir = "data"

## run all examples on Travis
# continuous_integration = [False for idx in range(len(exdirs))]
continuous_integration = [True, False]

# set replace_exe to None to use default executable
replace_exe = {"mf2005": "mf2005devdbl"}

htol = [None, None, None]
dtol = 1e-3

bud_lst = [
    "CSUB-ELASTIC_IN",
    "CSUB-INELASTIC_IN",
    "CSUB-ELASTIC_OUT",
    "CSUB-INELASTIC_OUT",
]

# static model data
nlay, nrow, ncol = 3, 10, 10
nper = 31
perlen = [1.0] + [365.2500000 for i in range(nper - 1)]
nstp = [1] + [6 for i in range(nper - 1)]
tsmult = [1.0] + [1.3 for i in range(nper - 1)]
steady = [True] + [False for i in range(nper - 1)]
delr, delc = 1000.0, 2000.0
top = 0.0
botm = [-100, -150.0, -350.0]
zthick = [top - botm[0], botm[0] - botm[1], botm[1] - botm[2]]
strt = 0.0
hnoflo = 1e30
hdry = -1e30

# calculate hk
hk1fact = 1.0 / zthick[1]
hk1 = np.ones((nrow, ncol), dtype=float) * 0.5 * hk1fact
hk1[0, :] = 1000.0 * hk1fact
hk1[-1, :] = 1000.0 * hk1fact
hk1[:, 0] = 1000.0 * hk1fact
hk1[:, -1] = 1000.0 * hk1fact
hk = [20.0, hk1, 5.0]

# calculate vka
vka = [1e6, 7.5e-5, 1e6]

# set rest of npf variables
laytyp = [0, 0, 0]  # [1, 0, 0]
ffrac = [0.6, 0.0, 0.6]
sy = [0.1, 0.0, 0.0]
ss = [3e-6, 0.0, 3e-6]

nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.0

tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# all cells are active
ib = 1

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

# static ibc and sub data
sgm = 0.0
sgs = 0.0
omega = 1.0

# no delay bed data
nndb = 3
lnd = [0, 1, 2]
hc = [-7.0, -7.0, -7.0]
thicknd0 = [15.0, 50.0, 30.0]
ccnd0 = [6e-4, 3e-4, 6e-4]
crnd0 = [6e-6, 3e-6, 6e-6]
sfv = []
sfe = []
for k in range(nlay):
    sfv.append(ccnd0[k] * thicknd0[k])
    sfe.append(crnd0[k] * thicknd0[k])

# delay bed data
nmz = 1
ldnd = [0, 2]
kv = 1e-6
void = 0.82
theta = void / (1.0 + void)
cc = 6e-4
cr = 6e-6
dp = [[kv, cr, cc]]
rnb = [7.635, 17.718]
dhc = [-7.0, -7.0]
dz = [5.894, 5.08]
nz = [1, 1]
dstart = []
for k in ldnd:
    pth = os.path.join(ddir, "ibc03_dstart{}.ref".format(k + 1))
    v = np.genfromtxt(pth)
    dstart.append(v.copy())

# sub output data
ds15 = [0, 0, 0, 2052, 0, 0, 0, 0, 0, 0, 0, 0]
ds16 = [0, nper - 1, 0, nstp[-1] - 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1]


# SUB package problem 3
def get_model(idx, dir):
    name = ex[idx]

    # ibc packagedata container counter
    sub6 = []
    ibcno = 0

    # create no delay bed packagedata entries
    if nndb > 0:
        cdelays = "nodelay"
        for kdx, k in enumerate(lnd):
            for i in range(nrow):
                for j in range(ncol):
                    # skip constant head cells
                    if k == 0 and i == nrow - 1 and j in ccol:
                        continue
                    tag = "{:02d}_{:02d}_{:02d}".format(k + 1, i + 1, j + 1)
                    # create nodelay entry
                    # no delay beds
                    b = thicknd0[kdx]
                    d = [
                        ibcno,
                        (k, i, j),
                        cdelays,
                        hc[idx],
                        b,
                        1.0,
                        ccnd0[kdx],
                        crnd0[kdx],
                        theta,
                        999.0,
                        -999.0,
                        tag,
                    ]
                    sub6.append(d)
                    ibcno += 1

    # create delay bed packagedata entries and coarse-grained material storage
    S = []
    ndb = ndelaybeds[idx]
    if ndb > 0:
        cdelays = "delay"
        for kdx, k in enumerate(ldnd):
            for i in range(nrow):
                for j in range(ncol):
                    # skip constant head cells
                    if k == 0 and i == nrow - 1 and j in ccol:
                        continue
                    tag = "{:02d}_{:02d}_{:02d}".format(k + 1, i + 1, j + 1)
                    # create nodelay entry
                    d = [
                        ibcno,
                        (k, i, j),
                        cdelays,
                        dhc[kdx],
                        dz[kdx],
                        rnb[kdx],
                        cc,
                        cr,
                        theta,
                        kv,
                        dstart[kdx][i, j],
                        tag,
                    ]
                    sub6.append(d)
                    ibcno += 1

        # create S for aquifer, delay beds, and no-delay beds
        for k in range(nlay):
            sst = (1.0 - ffrac[k]) * zthick[k] * ss[k]
            S.append(sst)
    else:
        # create S for aquifer and no-delay beds
        for k in range(nlay):
            sst = (zthick[k] - thicknd0[k]) * ss[k]
            S.append(sst)

    maxibc = len(sub6)

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
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)

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
        linear_acceleration="CG",
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
        filename="{}.dis".format(name),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename="{}.ic".format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        icelltype=laytyp,
        cvoptions=cvopt[idx],
        k=hk,
        k33=vka,
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        # ss=S, sy=sy,
        ss=0.0,
        sy=sy,
        storagecoefficient=True,
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
    # csub files
    opth = "{}.csub.obs".format(name)
    ibcsv = "{}.ib.strain.csv".format(name)
    skcsv = "{}.sk.strain.csv".format(name)
    copth = "{}.compaction.gridbin".format(name)
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        print_input=True,
        boundnames=True,
        head_based=True,
        effective_stress_lag=True,
        specified_initial_interbed_state=True,
        save_flows=True,
        strainib_filerecord=ibcsv,
        straincg_filerecord=skcsv,
        compaction_filerecord=copth,
        ndelaycells=ndelaycells[idx],
        ninterbeds=maxibc,
        beta=0.0,
        cg_ske_cr=ss,
        packagedata=sub6,
    )
    orecarray = {}
    orecarray["csub_obs.csv"] = [
        ("tcomp1", "interbed-compaction", "01_05_05"),
        ("tcomp2", "interbed-compaction", "02_05_05"),
        ("tcomp3", "interbed-compaction", "03_05_05"),
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
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # build MODFLOW-2005 files
    ws = os.path.join(dir, "mf2005")
    mc = flopy.modflow.Modflow(name, model_ws=ws)
    dis = flopy.modflow.ModflowDis(
        mc,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        nper=nper,
        perlen=perlen,
        nstp=nstp,
        tsmult=tsmult,
        steady=steady,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )
    bas = flopy.modflow.ModflowBas(
        mc, ibound=ib, strt=strt, hnoflo=hnoflo, stoper=0.01
    )
    lpf = flopy.modflow.ModflowLpf(
        mc,
        laytyp=laytyp,
        hk=hk,
        vka=vka,
        ss=S,
        sy=sy,
        constantcv=constantcv[idx],
        storagecoefficient=True,
        hdry=hdry,
    )
    chd = flopy.modflow.ModflowChd(mc, stress_period_data=cd)
    rch = flopy.modflow.ModflowRch(mc, rech=rech)
    wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
    sub = flopy.modflow.ModflowSub(
        mc,
        ndb=ndb,
        nndb=nndb,
        nmz=nmz,
        nn=20,
        idbit=1,
        ac2=omega,
        isuboc=1,
        ln=lnd,
        ldn=ldnd,
        rnb=rnb,
        dp=dp,
        dz=dz,
        nz=nz,
        dhc=dhc,
        dstart=dstart,
        hc=hc,
        sfe=sfe,
        sfv=sfv,
        ids15=ds15,
        ids16=ds16,
    )
    oc = flopy.modflow.ModflowOc(mc, stress_period_data=None)
    # sip = flopy.modflow.ModflowSip(mc, accl=1., mxiter=nouter, nparm=5,
    #                                hclose=hclose, ipcalc=1,
    #                                wseed=0., iprsip=1)
    pcg = flopy.modflow.ModflowPcg(
        mc,
        mxiter=nouter,
        iter1=ninner,
        hclose=hclose,
        rclose=rclose,
        relax=relax,
    )

    return sim, mc


def eval_comp(sim):
    print("evaluating compaction...")

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, "csub_obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # MODFLOW-2005 total compaction results
    fn = "{}.total_comp.hds".format(os.path.basename(sim.name))
    fpth = os.path.join(sim.simpath, "mf2005", fn)
    try:
        sobj = flopy.utils.HeadFile(fpth, text="LAYER COMPACTION")
        tc0 = sobj.get_ts((2, 4, 4))
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # calculate maximum absolute error
    diff = tc["TCOMP3"] - tc0[:, 1]
    diffmax = np.abs(diff).max()
    msg = "maximum absolute total-compaction difference ({}) ".format(diffmax)

    if diffmax > dtol:
        sim.success = False
        msg += "exceeds {}".format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    # get results from listing file
    fpth = os.path.join(
        sim.simpath, "{}.lst".format(os.path.basename(sim.name))
    )
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]

    # get results from cbc file
    cbc_bud = ["CSUB-ELASTIC", "CSUB-INELASTIC"]
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.0
    fpth = os.path.join(
        sim.simpath, "{}.cbc".format(os.path.basename(sim.name))
    )
    cobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.0
            qout = 0.0
            v = cobj.get_data(kstpkper=k, text=text)[0]
            for vv in v["q"]:
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

    if diffmax > dtol:
        sim.success = False
        msg += "exceeds {}".format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_input()
    return


def test_mf6model():
    # determine if running on Travis or GitHub actions
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        if is_CI and not continuous_integration[idx]:
            continue
        yield test.run_mf6, Simulation(
            dir, exfunc=eval_comp, exe_dict=r_exe, htol=htol[idx]
        )

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(
            dir, exfunc=eval_comp, exe_dict=replace_exe, htol=htol[idx]
        )
        test.run_mf6(sim)

    return


# use python test_gwf_csub_sub03.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
