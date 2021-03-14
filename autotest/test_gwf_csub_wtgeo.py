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

ex = [
    "csub_wtgeoa",
    "csub_wtgeob",
    "csub_wtgeoc",
    "csub_wtgeod",
    "csub_wtgeoe",
    "csub_wtgeof",
    "csub_wtgeog",
]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
constantcv = [True for idx in range(len(exdirs))]

cmppth = "mfnwt"
compare = [True, True, True, False, False, False, False]
tops = [0.0, 0.0, 150.0, 0.0, 0.0, 150.0, 150.0]
ump = [None, None, True, None, True, None, True]
iump = [0, 0, 1, 0, 1, 0, 1]
eslag = [True for idx in range(len(exdirs) - 2)] + 2 * [False]
# eslag = [True, True, True, False, True, False, False]
headformulation = [True, False, False, True, True, False, False]
ndc = [None, None, None, 19, 19, 19, 19]
delay = [False, False, False, True, True, True, True]
# newton = ["", "", "", "", "", None, ""]
newton = ["" for idx in range(len(exdirs))]

ddir = "data"

## run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

htol = [None, None, None, 0.2, None, None, None]
dtol = 1e-3
budtol = 1e-2
paktest = "csub"

# static model data
# temporal discretization
nper = 31
perlen = [1.0] + [365.2500000 for i in range(nper - 1)]
nstp = [1] + [6 for i in range(nper - 1)]
tsmult = [1.0] + [1.3 for i in range(nper - 1)]
steady = [True] + [False for i in range(nper - 1)]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# spatial discretization data
nlay, nrow, ncol = 3, 10, 10
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 1000.0, 2000.0
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

# solver options
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.0
# newtonoptions = None
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
sgm = 1.7
sgs = 2.0
void = 0.82
preconhead = -7.0
theta = void / (1.0 + void)
sw = 4.65120000e-10 * 9806.65000000 * theta
sy = [0.1, 0.0, 0.0]
ske = [6e-6, 3e-6, 6e-6]
skv = [6e-4, 3e-4, 6e-4]
cg_ske_cr = [ske[0], 0, ske[-1]]
kv = 1e-6

bdb = [45.0, 0, 90.0]
facib = [0.6, 1.0, 0.6]
facsk = [0.4, 0.0, 0.4]
# facndb = [0.15, 1., 0.15]
# facdb = [0.45, 0., 0.45]

dp = [[kv, ske[0], skv[0]]]
rnb = [7.635, 0.0, 17.718]
dhc = [preconhead for n in range(nlay)]
dstart = [strt for n in range(nlay)]
dz = [5.894, 0.0, 5.08]
nz = [1, 0, 1]

# sub output data
ds15 = [0, 0, 0, 2052, 0, 0, 0, 0, 0, 0, 0, 0]
ds16 = [0, nper - 1, 0, nstp[-1] - 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1]

# subwt output data
ds16swt = [
    0,
    0,
    0,
    2053,
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
ds17swt = [
    0,
    nper - 1,
    0,
    nstp[-1] - 1,
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


# calculate geostatic and effective stress
def calc_stress(sgm0, sgs0, h, bt):
    geo = []
    for k in range(nlay):
        top = bt[k]
        bot = bt[k + 1]
        ht = h
        if ht > top:
            gs = (top - bot) * sgs0
        elif ht < bot:
            gs = (top - bot) * sgm0
        else:
            gs = ((top - ht) * sgm0) + ((ht - bot) * sgs0)
        geo.append(gs)
    # calculate total geostatic stress at bottom of layer
    for k in range(1, nlay):
        geo[k] += geo[k - 1]
    # calculate effective stress at the bottom of the layer
    es = []
    for k in range(nlay):
        es.append(geo[k] - (h - bt[k + 1]))
    return geo, es


# variant SUB package problem 3
def get_model(idx, dir):
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

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
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

    # create gwf model
    top = tops[idx]
    zthick = [top - botm[0], botm[0] - botm[1], botm[1] - botm[2]]
    elevs = [top] + botm

    if top == 0:
        laytyp = [0, 0, 0]
    else:
        laytyp = [1, 0, 0]

    # calculate sk, ndb, and db factors
    # facndb = [0.15, 1., 0.15]
    # facdb = [0.45, 0., 0.45]
    # facsk = [0.4, 0., 0.4]
    facdb = []
    facndb = []
    for k in range(nlay):
        bt = zthick[k]
        f = bdb[k] / bt
        facdb.append(f)
        bnd = bt * (facib[k] - f)
        f = bnd / bt
        facndb.append(f)

    # csub packagedata container counter
    if headformulation[idx]:
        head_based = True
        sgmt = None
        sgst = None
    else:
        head_based = None
        sgmt = sgm
        sgst = sgs

    # fill preconsolidation stress with preconsolidation head
    # calculate preconsolidation stress, if necessary
    pcs = [preconhead for k in range(nlay)]
    gs, es = calc_stress(sgm, sgs, preconhead, elevs)
    if not headformulation[idx]:
        pcs = es

    # create no delay bed packagedata entries
    sub6 = []
    ibcno = 0
    nndb = 0
    ln = []
    sfv = []
    sfe = []
    hc = []
    thickib0 = []
    cdelays = "nodelay"
    for k in range(nlay):
        b = zthick[k] * facndb[k]
        if b <= 0.0:
            continue
        nndb += 1
        ln.append(k)
        thickib0.append(b)
        if headformulation[idx]:
            sfv.append(skv[k] * b)
            sfe.append(ske[k] * b)
            hc.append(pcs[k])
        else:
            sfv.append(skv[k])
            sfe.append(ske[k])
        for i in range(nrow):
            for j in range(ncol):
                # skip constant head cells
                if k == 0 and i == nrow - 1 and j in ccol:
                    continue
                # create nodelay entry
                # no delay beds
                d = [
                    ibcno,
                    (k, i, j),
                    cdelays,
                    pcs[k],
                    b,
                    1.0,
                    skv[k],
                    ske[k],
                    theta,
                    999.0,
                    -999.0,
                ]
                sub6.append(d)
                ibcno += 1

    if delay[idx]:
        cdelays = "delay"
    else:
        cdelays = "nodelay"
    ndb = 0
    nmz = 0
    ldn = []
    rnbsub = []
    nzsub = []
    dzsub = []
    dhcsub = []
    dstartsub = []
    for k in range(nlay):
        b = zthick[k] * facdb[k]
        if b <= 0.0:
            continue
        if delay[idx]:
            ndb += 1
            nmz = 1
            bb = dz[k]
            rnbt = rnb[k]
            rnbsub.append(rnbt)
            dzsub.append(dz[k])
            dhcsub.append(dhc[k])
            dstartsub.append(dstart[k])
            hib = strt
            nzsub.append(1)
            ldn.append(k)
        else:
            bb = b
            rnbt = 1
            hib = -999.0
            nndb += 1
            ln.append(k)
            thickib0.append(b)
            if headformulation[idx]:
                sfv.append(skv[k] * b)
                sfe.append(ske[k] * b)
                hc.append(pcs[k])
            else:
                sfv.append(skv[k])
                sfe.append(ske[k])
        for i in range(nrow):
            for j in range(ncol):
                # skip constant head cells
                if k == 0 and i == nrow - 1 and j in ccol:
                    continue
                # create nodelay entry
                # no delay beds
                d = [
                    ibcno,
                    (k, i, j),
                    cdelays,
                    pcs[k],
                    bb,
                    rnbt,
                    skv[k],
                    ske[k],
                    theta,
                    kv,
                    hib,
                ]
                sub6.append(d)
                ibcno += 1

    # add coarse-grained component
    for k in range(nlay):
        b = zthick[k] * facsk[k]
        if b <= 0.0:
            continue
        nndb += 1
        ln.append(k)
        thickib0.append(b)
        if headformulation[idx]:
            sfv.append(ske[k] * b)
            sfe.append(ske[k] * b)
            hc.append(pcs[k])
        else:
            sfv.append(ske[k])
            sfe.append(ske[k])

    maxcsub = len(sub6)

    # water compressibility cannot be compared for cases where the material
    # properties are adjusted since the porosity changes in mf6
    if iump[idx] == 0:
        beta = 4.6512e-10
        wc = sw
    else:
        beta = 0.0
        wc = 0.0

    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, newtonoptions=newton[idx])

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
    # ibc files
    opth = "{}.csub.obs".format(name)
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        specified_initial_interbed_state=True,
        ndelaycells=ndc[idx],
        head_based=head_based,
        update_material_properties=ump[idx],
        effective_stress_lag=eslag[idx],
        save_flows=True,
        ninterbeds=maxcsub,
        sgm=sgmt,
        sgs=sgst,
        cg_theta=theta,
        cg_ske_cr=cg_ske_cr,
        beta=beta,
        packagedata=sub6,
    )
    obspos = [(0, 4, 4), (1, 4, 4), (2, 4, 4)]
    obstype = [
        "compaction-cell",
        "gstress-cell",
        "estress-cell",
        "ske-cell",
        "sk-cell",
        "csub-cell",
    ]
    obstag = ["tcomp", "gs", "es", "ske", "sk", "csub"]
    obsarr = []
    for iobs, cobs in enumerate(obstype):
        for jobs, otup in enumerate(obspos):
            otag = "{}{}".format(obstag[iobs], jobs + 1)
            obsarr.append((otag, cobs, otup))

    orecarray = {}
    orecarray["csub_obs.csv"] = obsarr
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

    # build MODFLOW-NWT files
    if compare[idx]:
        cpth = cmppth
        ws = os.path.join(dir, cpth)
        mc = flopy.modflow.Modflow(name, model_ws=ws, version=cpth)
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
        upw = flopy.modflow.ModflowUpw(
            mc, laytyp=laytyp, hk=hk, vka=vka, ss=wc, sy=sy, hdry=hdry
        )
        chd = flopy.modflow.ModflowChd(mc, stress_period_data=cd)
        rch = flopy.modflow.ModflowRch(mc, rech=rech)
        wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
        if headformulation[idx]:
            sub = flopy.modflow.ModflowSub(
                mc,
                ipakcb=1001,
                ndb=ndb,
                nndb=nndb,
                nmz=nmz,
                nn=10,
                ac2=1.0,
                isuboc=1,
                ln=ln,
                ldn=ldn,
                rnb=rnbsub,
                dp=dp,
                dz=dzsub,
                nz=nzsub,
                dhc=dhcsub,
                dstart=dstartsub,
                hc=hc,
                sfe=sfe,
                sfv=sfv,
                ids15=ds15,
                ids16=ds16,
            )
        else:
            swt = flopy.modflow.ModflowSwt(
                mc,
                ipakcb=1001,
                iswtoc=1,
                nsystm=len(sfe),
                ithk=1,
                ivoid=iump[idx],
                icrcc=1,
                istpcs=0,
                lnwt=ln,
                sse=sfe,
                ssv=sfv,
                thick=thickib0,
                void=void,
                pcs=pcs,
                pcsoff=0.0,
                sgm=sgm,
                sgs=sgs,
                gl0=0.0,
                ids16=ds16swt,
                ids17=ds17swt,
            )
        oc = flopy.modflow.ModflowOc(
            mc,
            stress_period_data=None,
            save_every=1,
            save_types=["save head", "save budget", "print budget"],
        )
        fluxtol = (float(nlay * nrow * ncol) - 4.0) * rclose
        nwt = flopy.modflow.ModflowNwt(
            mc,
            headtol=hclose,
            fluxtol=fluxtol,
            maxiterout=nouter,
            linmeth=2,
            maxitinner=ninner,
            unitnumber=132,
            options="SPECIFIED",
            backflag=0,
            idroptol=0,
        )
    else:
        mc = None
    return sim, mc


def eval_comp(sim):

    if compare[sim.idxsim]:
        print("evaluating compaction...")
        # MODFLOW 6 total compaction results
        fpth = os.path.join(sim.simpath, "csub_obs.csv")
        try:
            tc = np.genfromtxt(fpth, names=True, delimiter=",")
        except:
            assert False, 'could not load data from "{}"'.format(fpth)

        # MODFLOW-2005 total compaction results
        cpth = cmppth
        fn2 = None
        if headformulation[sim.idxsim]:
            fn = "{}.total_comp.hds".format(os.path.basename(sim.name))
        else:
            fn = "{}.swt_total_comp.hds".format(os.path.basename(sim.name))
            if delay[sim.idxsim]:
                fn2 = "{}.total_comp.hds".format(os.path.basename(sim.name))
        fpth = os.path.join(sim.simpath, cpth, fn)
        try:
            sobj = flopy.utils.HeadFile(fpth, text="LAYER COMPACTION")
            tc0 = sobj.get_ts((2, 4, 4))
        except:
            assert False, 'could not load data from "{}"'.format(fpth)
        # add compaction from delay bed
        if fn2 is not None:
            fpth = os.path.join(sim.simpath, cpth, fn2)
            try:
                sobj = flopy.utils.HeadFile(fpth, text="LAYER COMPACTION")
                v = sobj.get_ts((2, 4, 4))
                tc0[:, 1] += v[:, 1]
            except:
                assert False, 'could not load data from "{}"'.format(fpth)

        # calculate maximum absolute error
        diff = tc["TCOMP3"] - tc0[:, 1]
        diffmax = np.abs(diff).max()
        msg = "maximum absolute total-compaction difference ({}) ".format(
            diffmax
        )

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
            line = "{:15g}".format(tc0[i, 0])
            line += " {:15g}".format(tc["TCOMP3"][i])
            line += " {:15g}".format(tc0[i, 1])
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
            dir, exfunc=eval_comp, exe_dict=r_exe, htol=htol[idx], idxsim=idx
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
            dir,
            exfunc=eval_comp,
            exe_dict=replace_exe,
            htol=htol[idx],
            idxsim=idx,
        )
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
