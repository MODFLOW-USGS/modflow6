import os
import sys
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

from framework import testing_framework, running_on_CI
from simulation import Simulation

ex = ['csub_subwt03a', 'csub_subwt03b', 'csub_subwt03c', 'csub_subwt03d']
nex = len(ex)
exdirs = [os.path.join('temp', s) for s in ex]

ddir = 'data'
cmppth = 'mf6'

htol = None #0.1
dtol = 1e-3
budtol = 1e-2

paktest = 'csub'

isnewton = 2 * [None, '']
headbased = [True, True, False, False]
delay = 4 * [False]

# set travis to True when version 1.13.0 is released
continuous_integration = [True for s in ex]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
pth = os.path.join(ddir, 'ibc01_ibound.ref')
ib0 = np.genfromtxt(pth)

# temporal discretization
nper = 3
perlen = [1., 21915., 21915.]
nstp = [1, 60, 60]
tsmult = [1., 1., 1.]
steady = [True, False, False]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# spatial discretization
nlay, nrow, ncol = 4, ib0.shape[0], ib0.shape[1]
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
nactive = np.count_nonzero(ib0) * nlay

delr, delc = 2000., 2000.
top = 150.
botm = [50., -100., -150., -350.]
strt = 100.

# create ibound/idomain
ib = []
for k in range(nlay):
    ib.append(ib0.astype(int).copy())

# upw data
laytyp = [1, 0, 0, 0]
hk = [4., 4., 1e-2, 4.]
sy = [0.3, 0., 0., 0.]

# build well stress period data
wnlays = [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
           0,  0,  0,  0,  0,  0,  0,  0,  1,  3]
wnrows = [ 0,  1,  1,  1,  2,  3,  4,  4,  5,  6,
           13, 13, 15, 15, 16, 17, 17, 18, 8, 11]
wncols = [ 7,  4,  7, 11,  3, 11,  2, 12, 13,  1,
           1, 13,  2, 12, 12,  3, 11,  6,  9, 6]
wrates0 = [2.2e+3 for n in range(18)] + [0., 0.]
wrates1 = [2.2e+3 for n in range(18)] + [-7.2e+04, -7.2e+04]

w0 = []
w1 = []
ws0 = []
ws1 = []
for idx, (k, i, j) in enumerate(zip(wnlays, wnrows, wncols)):
    w0.append((k, i, j, wrates0[idx]))
    w1.append((k, i, j, wrates1[idx]))
    ws0.append(((k, i, j), wrates0[idx]))
    ws1.append(((k, i, j), wrates1[idx]))
wd = {0: w0, 1: w1, 2: w0}
wd6 = {0: ws0, 1: ws1, 2: ws0}

# build chd stress period data
chead = 100.
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
cc = 0.25
cr = 0.25
void = 0.82
theta = void / (1. + void)
kv = 999.
sgm = 1.7
sgs = 2.0
thick = [45., 70., 50., 90.]

zthick = [top - botm[0],
          botm[0] - botm[1],
          botm[1] - botm[2],
          botm[2] - botm[3]]
zelv = np.array([top] + botm)

beta = 0.
# beta = 4.65120000e-10
gammaw = 9806.65000000

def get_ske():
    gsb = np.zeros((nlay), dtype=float)
    ub = np.zeros((nlay), dtype=float)
    esb = np.zeros((nlay), dtype=float)
    ske = np.zeros((nlay), dtype=float)

    # calculate incremental geostatic stress and hydrostatic stress
    for k in range(nlay):
        zt = zelv[k]
        zb = zelv[k+1]
        b = zthick[k]
        if strt >= zt:
            gs = b * sgs
        elif strt < zb:
            gs = b * sgm
        else:
            gs = (zt - strt) * sgm + (strt - zb) * sgs
        gsb[k] = gs
        ub[k] = strt - zb

    # calculate geostatic and effective stress at the bottom of the layer
    gsb = np.cumsum(gsb)
    esb = gsb - ub

    # calculate ske
    fact = 0.4342942
    ggammaw = 1. #gammaw * (60. * 60. * 24.)**2.
    for k in range(nlay):
        zt = zelv[k]
        zb = zelv[k+1]
        if strt >= zt:
            z = 0.5 * (zt + zb)
        elif strt < zb:
            z = zb
        else:
            z = 0.5 * (strt + zb)
        es = esb[k] - (z - zb) * (sgs - 1.)
        ske[k] = fact * cr * ggammaw / (es * (1 + void))

    return ske.tolist()


def get_interbed(headbased=False, delay=False):
    if headbased:
        ini_stress = strt - 15.
        cg_ske_cr = get_ske()
    else:
        ini_stress = 15.
        cg_ske_cr = [cr for k in range(nlay)]

    # create csub interbed data
    swt6 = []
    csubno = 0
    for k in range(nlay):
        bib = thick[k]
        rnb = 1.
        cdelay = 'nodelay'
        vk = kv
        if delay:
            vk = hk[k]
            if k != 2:
                rnb = bib / 5.
                bib = 5.
                cdelay = 'delay'
        for i in range(nrow):
            for j in range(ncol):
                iactive = 0
                if ib0[i, j] > 0:
                    iactive = 1
                if i == 19 and (j == 7 or j == 8):
                    iactive = 0
                if iactive > 0:
                    tag = '{:02d}_{:02d}_{:02d}'.format(k + 1, i + 1, j + 1)
                    d = [csubno, (k, i, j), cdelay, ini_stress, bib,
                         rnb, cg_ske_cr[k], cg_ske_cr[k], theta,
                         vk, strt, tag]
                    swt6.append(d)
                    csubno += 1
    return swt6


def get_model(idx, dir):
    sim = build_mf6(idx, dir)

    # build mf6 with interbeds
    wsc = os.path.join(dir, 'mf6')
    mc = build_mf6(idx, wsc, interbed=True)

    return sim, mc


# build MODFLOW 6 files
def build_mf6(idx, ws, interbed=False):

    name = ex[idx]
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True,
                               newtonoptions=isnewton[idx])

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               outer_dvclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='NONE',
                               inner_maximum=ninner,
                               inner_dvclose=hclose, rcloserecord=rclose,
                               linear_acceleration='BICGSTAB',
                               scaling_method='NONE',
                               reordering_method='NONE',
                               relaxation_factor=relax)
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm,
                                  idomain=ib,
                                  filename='{}.dis'.format(name))

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                filename='{}.ic'.format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=laytyp,
                                  k=hk,
                                  k33=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False, iconvert=laytyp,
                                  ss=0., sy=sy,
                                  steady_state={0: True},
                                  transient={1: True})

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                   maxbound=len(chd6),
                                                   stress_period_data=cd6,
                                                   save_flows=False)

    # wel files
    wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
                                  maxbound=len(ws1),
                                  stress_period_data=wd6,
                                  save_flows=False)

    # csub files
    if interbed:
        sswt6 = get_interbed(headbased=headbased[idx], delay=delay[idx])
        ninterbeds = len(sswt6)
    else:
        sswt6 = None
        ninterbeds = 0
    if headbased[idx]:
        eslag = True
        ci = None
        hb = True
        ssgs = None
        ssgm = None
        cg_ske_cr = get_ske()
    else:
        eslag = True
        ci = True
        hb = None
        ssgs = sgs
        ssgm = sgm
        cg_ske_cr = [cr for k in range(nlay)]
    if interbed:
        cg_ske_cr[2] = 0
    opth = '{}.csub.obs'.format(name)
    csub = flopy.mf6.ModflowGwfcsub(gwf,
                                    print_input=True,
                                    effective_stress_lag=eslag,
                                    head_based=hb,
                                    boundnames=True,
                                    compression_indices=ci,
                                    ninterbeds=ninterbeds,
                                    sgs=ssgs, sgm=ssgm,
                                    beta=beta,
                                    gammaw=gammaw,
                                    cg_ske_cr=cg_ske_cr,
                                    cg_theta=theta,
                                    packagedata=sswt6)
    orecarray = {}
    orecarray['csub_obs.csv'] = [('wc01', 'compaction-cell', (1, 5,  8)),
                                 ('wc02', 'compaction-cell', (3, 6, 11))]
    csub_obs_package = csub.obs.initialize(filename=opth, digits=10,
                                           print_input=True,
                                           continuous=orecarray)

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'ALL'),
                                             ('BUDGET', 'ALL')])
    return sim


def eval_comp(sim):
    print('evaluating compaction...')

    # MODFLOW 6 without interbeds
    fpth = os.path.join(sim.simpath, 'csub_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # MODFLOW 6 with interbeds
    fpth = os.path.join(sim.simpath, cmppth, 'csub_obs.csv')
    try:
        tci = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    diffmax = 0.
    tagmax = None
    for tag in tc.dtype.names[1:]:
        diff = tc[tag] - tci[tag]
        diffmaxt = np.abs(diff).max()
        if diffmaxt > diffmax:
            diffmax = diffmaxt
            tagmax = tag

    msg = 'maximum compaction difference ' + \
          '({}) in tag: {}'.format(diffmax, tagmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.comp.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    line = '{:>15s}'.format('TOTIM')
    for tag in tc.dtype.names[1:]:
        line += ' {:>15s}'.format('{}_SK'.format(tag))
        line += ' {:>15s}'.format('{}_SKIB'.format(tag))
        line += ' {:>15s}'.format('{}_DIFF'.format(tag))
    f.write(line + '\n')
    for i in range(diff.shape[0]):
        line = '{:15g}'.format(tc['time'][i])
        for tag in tc.dtype.names[1:]:
            line += ' {:15g}'.format(tc[tag][i])
            line += ' {:15g}'.format(tci[tag][i])
            line += ' {:15g}'.format(tc[tag][i]-tci[tag][i])
        f.write(line + '\n')
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    # compare budgets
    cbc_compare(sim)

    return


# compare cbc and lst budgets
def cbc_compare(sim):
    print('evaluating cbc and budget...')
    # open cbc file
    fpth = os.path.join(sim.simpath,
                        '{}.cbc'.format(os.path.basename(sim.name)))
    cobj = flopy.utils.CellBudgetFile(fpth, precision='double')

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
            bud_lst.append('{}_IN'.format(t))
            bud_lst.append('{}_OUT'.format(t))

    # get results from listing file
    fpth = os.path.join(sim.simpath,
                        '{}.lst'.format(os.path.basename(sim.name)))
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.

    # get data from cbc dile
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.
            qout = 0.
            v = cobj.get_data(kstpkper=k, text=text)[0]
            if isinstance(v, np.recarray):
                vt = np.zeros(size3d, dtype=float)
                for jdx, node in enumerate(v['node']):
                    vt[node - 1] += v['q'][jdx]
                v = vt.reshape(shape3d)
            for kk in range(v.shape[0]):
                for ii in range(v.shape[1]):
                    for jj in range(v.shape[2]):
                        vv = v[kk, ii, jj]
                        if vv < 0.:
                            qout -= vv
                        else:
                            qin += vv
            d['totim'][idx] = t
            d['time_step'][idx] = k[0]
            d['stress_period'] = k[1]
            key = '{}_IN'.format(text)
            d[key][idx] = qin
            key = '{}_OUT'.format(text)
            d[key][idx] = qout

    diff = np.zeros((nbud, len(bud_lst)), dtype=float)
    for idx, key in enumerate(bud_lst):
        diff[:, idx] = d0[key] - d[key]
    diffmax = np.abs(diff).max()
    msg = 'maximum absolute total-budget difference ({}) '.format(diffmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.bud.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    for i in range(diff.shape[0]):
        if i == 0:
            line = '{:>10s}'.format('TIME')
            for idx, key in enumerate(bud_lst):
                line += '{:>25s}'.format(key + '_LST')
                line += '{:>25s}'.format(key + '_CBC')
                line += '{:>25s}'.format(key + '_DIF')
            f.write(line + '\n')
        line = '{:10g}'.format(d['totim'][i])
        for idx, key in enumerate(bud_lst):
            line += '{:25g}'.format(d0[key][i])
            line += '{:25g}'.format(d[key][i])
            line += '{:25g}'.format(diff[i, idx])
        f.write(line + '\n')
    f.close()

    if diffmax > budtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_simulation()
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
        yield test.run_mf6, Simulation(dir, exe_dict=r_exe,
                                       exfunc=eval_comp,
                                       cmp_verbose=False,
                                       htol=htol)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for dir in exdirs:
        sim = Simulation(dir, exe_dict=replace_exe,
                         exfunc=eval_comp,
                         cmp_verbose=False,
                         htol=htol)
        test.run_mf6(sim)

    return


# use python testmf6_csub_subwt03.py
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
