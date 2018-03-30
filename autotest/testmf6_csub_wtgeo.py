import os
import numpy as np

try:
    import pymake
except:
    msg = 'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    raise Exception(msg)

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

import targets
from framework import testing_framework
from simulation import Simulation

ex = ['ibcwtgeoa', 'ibcwtgeob']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))

ssnam = 'ibcwtgeo_ss'
ssdir = os.path.join('temp', ssnam)

ddir = 'data'

## run all examples on Travis
# travis = [False for idx in range(len(exdirs))]
travis = [False, False]

mf2005 = [True, False]

# set replace_exe to None to use default executable
replace_exe = {'mf2005': 'mf2005devdbl'}

htol = [None, None, None]
dtol = 1e-3

# static model data
hnoflo = 1e30
hdry = -1e30

nlay, nrow, ncol = 3, 10, 10
nper = 31
perlen = [1] + [365.2500000 for i in range(nper - 1)]
nstp = [1] + [6 for i in range(nper - 1)]
tsmult = [1] + [1.3 for i in range(nper - 1)]
steady = [True] + [False for i in range(nper - 1)]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# dis data
delr, delc = 1000., 2000.
top = np.ones((nrow, ncol), dtype=np.float) * 25.
botm = [-100, -150., -350.]

# all cells are active
ib = 1

# npf/lpf data
laytyp = [1, 0, 0]
cvopt = [None, None]
constantcv = [True, True]
hk1fact = 1. / 100.
hk1 = np.ones((nrow, ncol), dtype=np.float) * 0.5 * hk1fact
hk1[0, :] = 1000. * hk1fact
hk1[-1, :] = 1000. * hk1fact
hk1[:, 0] = 1000. * hk1fact
hk1[:, -1] = 1000. * hk1fact
hk = [20., hk1, 5.]
vka = [1e6, 7.5e-5, 1e6]

# storage data
sy = [0.3, 0., 0.]
ss = [0., 0., 0.]

# chd data
c = []
c6 = []
ccol = [3, 4, 5, 6]
chdh = 0.
for j in ccol:
    c.append([0, nrow - 1, j, chdh, chdh])
    c6.append([(0, nrow - 1, j), chdh])
cd = {0: c}
cd6 = {0: c6}
maxchd = len(cd[0])

# pumping well data
wr = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3]
wc = [0, 1, 8, 9, 0, 9, 0, 9, 0, 9]
wrp = [2, 2, 3, 3]
wcp = [5, 6, 5, 6]
wq = [-14000., -8000., -5000., -3000.]
d = []
d6 = []
for r, c, q in zip(wrp, wcp, wq):
    d.append([2, r, c, q])
    d6.append([(2, r, c), q])
wd = {1: d}
wd6 = {1: d6}
maxwel = len(wd[1])

# recharge data
q = 3000. / (delr * delc)
v = np.zeros((nrow, ncol), dtype=np.float)
for r, c in zip(wr, wc):
    v[r, c] = q
rech = {0: v}

# ibc data
fullcell = [None, True]
ndelaycells = [20, 39]
sgm = 1.7
sgs = 2.
compind = [None, True]
storagecoeff = [True, None]
cr = 0.01
cc = 0.25
void = 0.82
theta = void / (1. + void)

# sub output data
ds15 = [0, 0, 0, 2052, 0, 0, 0, 0, 0, 0, 0, 0]
ds16 = [0, nper - 1, 0, nstp[-1] - 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1]

# solver data
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.


# calculate geostatic and effective stress
def calc_stress(sgm0, sgs0, h, bt):
    znode = np.zeros((nlay, nrow, ncol), dtype=np.float)
    bb = np.zeros((nlay, nrow, ncol), dtype=np.float)
    geo = np.zeros((nlay, nrow, ncol), dtype=np.float)
    for k in range(nlay):
        znode[k, :, :] = 0.5 * (bt[k, :, :] + bt[k + 1, :, :])
        bb[k, :, :] = (bt[k, :, :] - bt[k+1, :, :])
        for i in range(nrow):
            for j in range(ncol):
                top = bt[k, i, j]
                bot = bt[k + 1, i, j]
                ht = h[k, i, j]
                if ht >= top:
                    gs = (top - bot) * sgs0
                elif ht <= bot:
                    gs = (top - bot) * sgm0
                else:
                    gs = ((top - ht) * sgm0) + ((ht - bot) * sgs0)
                geo[k, i, j] = gs
    # calculate total geostatic stress at bottom of layer
    for k in range(1, nlay):
        geo[k, :, :] += geo[k - 1, :, :]
    # # adjust total geostatic stress to node
    # geo -= sgs0 * bb * 0.5
    # calculate effective stress at the bottom of the layer
    es = np.zeros((nlay, nrow, ncol), dtype=np.float)
    for k in range(nlay):
        es = geo - (h - bt[1:, :, :])

    return geo, es


# based on SUB package problem 3
def build_models():
    # set heads to top of model in all layers
    h0 = np.ones((nlay, nrow, ncol), dtype=np.float) * 25.
    strt = []
    for k in range(nlay):
        strt.append(h0[k, :, :].copy())

    zthick = np.zeros((nlay, nrow, ncol), dtype=np.float)
    zthick[0, :, :] = top - botm[0]
    for k in range(1, nlay):
        zthick[k] = botm[k - 1] - botm[k]

    botarray = np.zeros((nlay + 1, nrow, ncol), dtype=np.float)
    botarray[0, :, :] = top.copy()
    for k in range(nlay):
        botarray[k + 1, :, :] = botm[k]

    # no delay bed data
    nndb = 5
    nd0 = 3
    lnd = [0, 1, 2, 0, 2]
    thicknd0 = [75., 50., 30.]
    thicknd1 = [50., 0., 80.]
    hc = []
    for k in lnd:
        # hc.append(h0[k, 0, 0])
        hc.append(0.)

    # delay bed data
    ndb = 1
    ldnd = [2] #[1, 2]
    nmz = 1 #2
    kv = [1e-6] #[7.5e-5, 1e-6]
    rnb = [17.718] #[1., 17.718]
    dz = [5.08] #[50., 5.08]
    nz = [1] #[1, 2]
    dhc = [0.]
    dstart = []
    for k in ldnd:
        dstart.append(h0[k, :, :].copy())

    for idx, dir in enumerate(exdirs):
        name = ex[idx]

        # ibc packagedata container counter
        sub6 = []
        ibcno = 0

        gs, pcs0 = calc_stress(sgm, sgs, h0, botarray)

        ndssv = []
        ndsse = []
        ndskv = []
        ndske = []
        thicknd = []
        if storagecoeff[idx]:
            tsgs = None
            tsgm = None
            head_based = True
            for k in lnd[:nd0]:
                b = thicknd0[k]
                bb = zthick[k, 0, 0] * 0.5
                denom = (1. + void) * (pcs0[k, 0, 0] - bb) * (sgs - 1.)
                f = 0.434 / denom
                dskv = cc * f
                dske = cr * f
                ndskv.append(dskv * b)
                ndske.append(dske * b)
                ndssv.append(dskv)
                ndsse.append(dske)
                thicknd.append(b)
            for k in lnd[nd0:]:
                b = thicknd1[k]
                bb = zthick[k, 0, 0] * 0.5
                denom = (1. + void) * (pcs0[k, 0, 0] - bb) * (sgs - 1.)
                f = 0.434 / denom
                dske = cr * f
                ndskv.append(dske * b)
                ndske.append(dske * b)
                ndssv.append(dske)
                ndsse.append(dske)
                thicknd.append(b)
            # reset pcs to initial heads
            pcs0 = h0.copy()
        else:
            tsgs = sgs
            tsgm = sgm
            head_based = None
            dskv = cc
            dske = cr
            for k in lnd[:nd0]:
                b = thicknd0[k]
                ndssv.append(cc)
                ndsse.append(cr)
                thicknd.append(b)
            for k in lnd[nd0:]:
                b = thicknd1[k]
                ndssv.append(cr)
                ndsse.append(cr)
                thicknd.append(b)

        # material data for delay beds
        # dp = [[kv[0], dske * dfac[0], dskv * dfac[0]],
        #       [kv[1], dske * dfac[1], dskv * dfac[1]]]
        dskv = ndssv[nd0-1]
        dske = ndsse[nd0-1]
        dp = [[kv[0], dske, dskv]]

        # create no delay bed packagedata entries
        if nndb > 0:
            cdelays = 'nodelay'
            for kdx, k in enumerate(lnd):
                for i in range(nrow):
                    for j in range(ncol):
                        # skip constant head cells
                        if k == 0 and i == nrow - 1 and j in ccol:
                            continue
                        # create nodelay entry
                        # no delay beds
                        ibcno += 1
                        b = thicknd[kdx]
                        d = [ibcno, (k, i, j), cdelays, hc[kdx],
                             b, 999., ndssv[kdx], ndsse[kdx], theta, 999., -999.]
                        sub6.append(d)

        # create delay bed packagedata entries
        if ndb > 0:
            cdelays = 'delay'
            pcs0 = np.ones((nlay, nrow, ncol), dtype=np.float) * dhc[0]
            if not storagecoeff[idx]:
                gs, pcs0 = calc_stress(sgm, sgs, pcs0, botarray)
            for kdx, k in enumerate(ldnd):
                for i in range(nrow):
                    for j in range(ncol):
                        # skip constant head cells
                        if k == 0 and i == nrow - 1 and j in ccol:
                            continue
                        # create nodelay entry
                        ibcno += 1
                        d = [ibcno, (k, i, j), cdelays, pcs0[k, i, j], dz[kdx],
                             rnb[kdx], dskv, dske,
                             theta, kv[kdx], h0[k, i, j]]
                        sub6.append(d)

        maxibc = len(sub6)

        # build MODFLOW 6 files
        ws = dir
        sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                     exe_name='mf6',
                                     sim_ws=ws)
        # create tdis package
        tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                     nper=nper, perioddata=tdis_rc)

        # create gwf model
        gwf = flopy.mf6.ModflowGwf(sim, modelname=name)

        # create iterative model solution and register the gwf model with it
        ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                                   outer_hclose=hclose,
                                   outer_maximum=nouter,
                                   under_relaxation='NONE',
                                   inner_maximum=ninner,
                                   inner_hclose=hclose, rcloserecord=rclose,
                                   linear_acceleration='CG',
                                   scaling_method='NONE',
                                   reordering_method='NONE',
                                   relaxation_factor=relax)
        sim.register_ims_package(ims, [gwf.name])

        dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                      delr=delr, delc=delc,
                                      top=top, botm=botm,
                                      fname='{}.dis'.format(name))

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                    fname='{}.ic'.format(name))

        # head observations
        hrecarray = {}
        hstr = 'HEAD'
        hrecarray['head_obs.csv'] = [('hobs1', hstr, (0, 4, 4)),
                                     ('hobs2', hstr, (1, 4, 4)),
                                     ('hobs3', hstr, (2, 4, 4))]
        hobs = flopy.mf6.ModflowUtlobs(gwf, continuous=hrecarray)

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                      icelltype=laytyp,
                                      cvoptions=cvopt[idx],
                                      k=hk,
                                      k33=vka)
        # storage
        sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False, iconvert=laytyp,
                                      ss=ss, sy=sy,
                                      storagecoefficient=True,
                                      steady_state={0: True},
                                      transient={1: True})

        # recharge
        rch = flopy.mf6.ModflowGwfrcha(gwf, readasarrays=True, recharge=rech)

        # wel file
        wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
                                      maxbound=maxwel,
                                      stress_period_data=wd6,
                                      save_flows=False)

        # chd files
        chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                       maxbound=maxchd,
                                                       stress_period_data=cd6,
                                                       save_flows=False)
        # ibc files
        opth = '{}.ibc.obs'.format(name)
        ibc = flopy.mf6.ModflowGwfcsub(gwf, head_based=head_based,
                                       ndelaycells=ndelaycells[idx],
                                       delay_full_cell=fullcell[idx],
                                       compression_indices=compind[idx],
                                       constant_nodelay_thickness=True,
                                       ninterbeds=maxibc,
                                       obs_filerecord=opth,
                                       ske_cr=0., sgs=tsgs, sgm=tsgm,
                                       packagedata=sub6)
        orecarray = {}
        tcstr = 'total-compaction'
        esstr = 'estress-cell'
        gsstr = 'gstress-cell'
        orecarray['ibc_obs.csv'] = [('tcomp1', tcstr, (0, 4, 4)),
                                    ('tcomp2', tcstr, (1, 4, 4)),
                                    ('tcomp3', tcstr, (2, 4, 4)),
                                    ('es1', esstr, (0, 4, 4)),
                                    ('es2', esstr, (1, 4, 4)),
                                    ('es3', esstr, (2, 4, 4)),
                                    ('gs1', gsstr, (0, 4, 4)),
                                    ('gs2', gsstr, (1, 4, 4)),
                                    ('gs3', gsstr, (2, 4, 4)),
                                    ]
        ibc_obs_package = flopy.mf6.ModflowUtlobs(gwf,
                                                  fname=opth,
                                                  parent_file=ibc, digits=10,
                                                  print_input=True,
                                                  continuous=orecarray)

        # output control
        oc = flopy.mf6.ModflowGwfoc(gwf,
                                    budget_filerecord='{}.cbc'.format(name),
                                    head_filerecord='{}.hds'.format(name),
                                    headprintrecord=[
                                        ('COLUMNS', 10, 'WIDTH', 15,
                                         'DIGITS', 6, 'GENERAL')],
                                    saverecord=[('HEAD', 'LAST')],
                                    printrecord=[('HEAD', 'LAST'),
                                                 ('BUDGET', 'LAST')])

        # write MODFLOW 6 files
        sim.write_simulation()

        # build MODFLOW-2005 files
        if mf2005[idx]:
            ws = os.path.join(dir, 'mf2005')
            mc = flopy.modflow.Modflow(name, model_ws=ws)
            dis = flopy.modflow.ModflowDis(mc, nlay=nlay, nrow=nrow, ncol=ncol,
                                           nper=nper, perlen=perlen, nstp=nstp,
                                           tsmult=tsmult, steady=steady,
                                           delr=delr,
                                           delc=delc, top=top, botm=botm)
            bas = flopy.modflow.ModflowBas(mc, ibound=ib, strt=strt,
                                           hnoflo=hnoflo)
            lpf = flopy.modflow.ModflowLpf(mc, laytyp=laytyp, hk=hk, vka=vka,
                                           ss=ss, sy=sy,
                                           constantcv=constantcv[idx],
                                           storagecoefficient=True,
                                           hdry=hdry)
            chd = flopy.modflow.ModflowChd(mc, stress_period_data=cd)
            rch = flopy.modflow.ModflowRch(mc, rech=rech)
            wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
            sub = flopy.modflow.ModflowSub(mc, nndb=nndb, ln=lnd,
                                           sfe=ndske, sfv=ndskv, hc=hc,
                                           ndb=ndb, nmz=nmz, nn=20, ac2=1.0,
                                           ldn=ldnd, rnb=rnb,
                                           dp=dp, dz=dz, nz=nz,
                                           dhc=dhc, dstart=dstart,
                                           isuboc=1, ids15=ds15, ids16=ds16)
            oc = flopy.modflow.ModflowOc(mc, stress_period_data=None)
            pcg = flopy.modflow.ModflowPcg(mc, mxiter=nouter, iter1=ninner,
                                           hclose=hclose, rclose=rclose,
                                           relax=relax)
            mc.write_input()

    return


def eval_comp(sim):
    print('evaluating compaction...')

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, 'ibc_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    if mf2005[sim.idxsim]:
        # MODFLOW-2005 total compaction results
        fn = '{}.total_comp.hds'.format(os.path.basename(sim.name))
        fpth = os.path.join(sim.simpath, 'mf2005', fn)
        try:
            sobj = flopy.utils.HeadFile(fpth, text='LAYER COMPACTION')
            ts = sobj.get_ts((2, 4, 4))
            tc0 = ts[:, 1]
        except:
            assert False, 'could not load data from "{}"'.format(fpth)
    else:
        tc0 = tc['TCOMP3']


    # calculate maximum absolute error
    diff = tc['TCOMP3'] - tc0
    diffmax = np.abs(diff).max()
    msg = 'maximum absolute total-compaction difference ({}) '.format(diffmax)

    if diffmax > dtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    return


# - No need to change any code below
def test_mf6model():

    # determine if running on Travis
    is_travis = 'TRAVIS' in os.environ
    r_exe = None
    if not is_travis:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        if is_travis and not travis[idx]:
            continue
        yield test.run_mf6, Simulation(dir, exfunc=eval_comp,
                                       exe_dict=r_exe,
                                       htol=htol[idx],
                                       idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_comp, exe_dict=replace_exe,
                         htol=htol[idx], idxsim=idx)
        test.run_mf6(sim)

    return


# use python testmf6_csub_sub03.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
