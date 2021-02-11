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

from framework import testing_framework, running_on_CI
from simulation import Simulation

ex = ['csub_sk02a', 'csub_sk02b', 'csub_sk02c', 'csub_sk02d']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
constantcv = [True for idx in range(len(exdirs))]

cmppths = ['mfnwt' for idx in range(len(exdirs))]
tops = [150. for idx in range(len(exdirs))]
newtons = [True for idx in range(len(exdirs))]
ump = [None, None, True, True]
iump = [0, 0, 1, 1]
eslag = [True for idx in range(len(exdirs))]
icrcc = [0, 1, 0, 1]

ddir = 'data'

## run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

htol = [None for idx in range(len(exdirs))]
dtol = 1e-3

bud_lst = ['CSUB-CGELASTIC_IN', 'CSUB-CGELASTIC_OUT',
           'CSUB-WATERCOMP_IN', 'CSUB-WATERCOMP_OUT']

# static model data
nlay, nrow, ncol = 3, 10, 10
nper = 31
perlen = [1.] + [365.2500000 for i in range(nper - 1)]
nstp = [1] + [6 for i in range(nper - 1)]
tsmult = [1.0] + [1.3 for i in range(nper - 1)]
steady = [True] + [False for i in range(nper - 1)]
delr, delc = 1000., 2000.
top = 150.
botm = [-100, -150., -350.]
zthick = [top - botm[0],
          botm[0] - botm[1],
          botm[1] - botm[2]]
strt = 100.
hnoflo = 1e30
hdry = -1e30

# calculate hk
hk1fact = 1. / zthick[1]
hk1 = np.ones((nrow, ncol), dtype=float) * 0.5 * hk1fact
hk1[0, :] = 1000. * hk1fact
hk1[-1, :] = 1000. * hk1fact
hk1[:, 0] = 1000. * hk1fact
hk1[:, -1] = 1000. * hk1fact
hk = [20., hk1, 5.]

# calculate vka
vka = [1e6, 7.5e-5, 1e6]

# set rest of npf variables
laytyp = [1, 0, 0]
laytypu = [4, 0, 0]
sy = [0.1, 0., 0.]

nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.

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
v = np.zeros((nrow, ncol), dtype=float)
for r, c in zip(wr, wc):
    v[r, c] = q
rech = {0: v}

# static ibc and sub data
sgm = 1.7
sgs = 2.0
void = 0.82
ini_stress = 15.0
theta = void / (1. + void)
sw = 4.65120000e-10 * 9806.65000000 * theta

# no delay bed data
lnd = [0, 1, 2]
thicknd0 = [zthick[0], zthick[1], zthick[2]]
cr = [0.01, 0.005, 0.01]
sske = [6e-6, 3e-6, 6e-6]

# subwt output data
ds16 = [0, 0, 0, 2052, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
ds17 = [0, 10000, 0, 10000, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


# SUB package problem 3
def get_model(idx, dir):
    name = ex[idx]
    newton = newtons[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    # create gwf model
    newtonoptions = None
    imsla = 'CG'
    if newton:
        newtonoptions = ''
        imsla = 'BICGSTAB'
    if icrcc[idx] == 0:
        sc = cr
        compression_indices = True
    else:
        sc = sske
        compression_indices = None
    # water compressibility cannot be compared for cases where the material
    # properties are adjusted since the porosity changes in mf6
    if iump[idx] == 0:
        beta = 4.6512e-10
        wc = sw
    else:
        beta = 0.
        wc = 0.

    gwf = flopy.mf6.ModflowGwf(sim, modelname=name,
                               newtonoptions=newtonoptions)

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               outer_dvclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='NONE',
                               inner_maximum=ninner,
                               inner_dvclose=hclose, rcloserecord=rclose,
                               linear_acceleration=imsla,
                               scaling_method='NONE',
                               reordering_method='NONE',
                               relaxation_factor=relax)
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm,
                                  filename='{}.dis'.format(name))

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                filename='{}.ic'.format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  # dev_modflowusg_upstream_weighted_saturation=True,
                                  icelltype=laytyp,
                                  k=hk,
                                  k33=vka)
    # storage
    sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False, iconvert=laytyp,
                                  ss=0., sy=sy,
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
    opth = '{}.csub.obs'.format(name)
    csub = flopy.mf6.ModflowGwfcsub(gwf,
                                    update_material_properties=ump[idx],
                                    effective_stress_lag=eslag[idx],
                                    save_flows=True,
                                    ninterbeds=0,
                                    compression_indices=compression_indices,
                                    sgm=sgm,
                                    sgs=sgs,
                                    cg_theta=theta,
                                    cg_ske_cr=sc,
                                    beta=beta,
                                    packagedata=None)
    orecarray = {}
    orecarray['csub_obs.csv'] = [('tcomp1', 'compaction-cell', (0, 4, 4)),
                                 ('tcomp2', 'compaction-cell', (1, 4, 4)),
                                 ('tcomp3', 'compaction-cell', (2, 4, 4))]
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
                                saverecord=[('HEAD', 'LAST'),
                                            ('BUDGET', 'LAST')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # build MODFLOW-2005 files
    cpth = cmppths[idx]
    ws = os.path.join(dir, cpth)
    mc = flopy.modflow.Modflow(name, model_ws=ws, version=cpth)
    dis = flopy.modflow.ModflowDis(mc, nlay=nlay, nrow=nrow, ncol=ncol,
                                   nper=nper, perlen=perlen, nstp=nstp,
                                   tsmult=tsmult, steady=steady, delr=delr,
                                   delc=delc, top=top, botm=botm)
    bas = flopy.modflow.ModflowBas(mc, ibound=ib, strt=strt, hnoflo=hnoflo,
                                   stoper=0.01)
    if newton:
        if cpth == 'mfnwt':
            upw = flopy.modflow.ModflowUpw(mc, laytyp=laytyp,
                                           hk=hk, vka=vka,
                                           ss=wc, sy=sy,
                                           hdry=hdry)
        else:
            lpf = flopy.modflow.ModflowLpf(mc, laytyp=laytypu,
                                           hk=hk, vka=vka,
                                           ss=wc, sy=sy,
                                           hdry=hdry, constantcv=True)
    else:
        lpf = flopy.modflow.ModflowLpf(mc, laytyp=laytyp, hk=hk, vka=vka,
                                       ss=sw, sy=sy,
                                       constantcv=constantcv[idx],
                                       storagecoefficient=False,
                                       hdry=hdry)
    chd = flopy.modflow.ModflowChd(mc, stress_period_data=cd)
    rch = flopy.modflow.ModflowRch(mc, rech=rech)
    wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
    swt = flopy.modflow.ModflowSwt(mc, iswtoc=1, nsystm=3,
                                   ithk=1, ivoid=iump[idx],
                                   icrcc=icrcc[idx],
                                   istpcs=1, lnwt=lnd,
                                   cc=sc, cr=sc,
                                   sse=sc, ssv=sc,
                                   thick=thicknd0,
                                   void=void, pcsoff=ini_stress, sgs=sgs,
                                   gl0=0., ids16=ds16, ids17=ds17)
    oc = flopy.modflow.ModflowOc(mc, stress_period_data=None)
    if newton:
        if cpth == 'mfnwt':
            fluxtol = (float(nlay * nrow * ncol) - 4.) * rclose
            nwt = flopy.modflow.ModflowNwt(mc,
                                           headtol=hclose, fluxtol=fluxtol,
                                           maxiterout=nouter, linmeth=2,
                                           maxitinner=ninner,
                                           unitnumber=132,
                                           options='SPECIFIED',
                                           backflag=0, idroptol=0)
        else:
            sms = flopy.modflow.ModflowSms(mc, hclose=hclose,
                                           hiclose=hclose,
                                           mxiter=nouter, iter1=ninner,
                                           rclosepcgu=rclose,
                                           relaxpcgu=relax,
                                           unitnumber=132)
    else:
        pcg = flopy.modflow.ModflowPcg(mc, mxiter=nouter, iter1=ninner,
                                       hclose=hclose, rclose=rclose,
                                       relax=relax)

    return sim, mc


def eval_comp(sim):
    print('evaluating compaction...')

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, 'csub_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # MODFLOW-2005 total compaction results
    cpth = cmppths[sim.idxsim]
    fn = '{}.swt_total_comp.hds'.format(os.path.basename(sim.name))
    fpth = os.path.join(sim.simpath, cpth, fn)
    try:
        sobj = flopy.utils.HeadFile(fpth, text='LAYER COMPACTION')
        tc0 = sobj.get_ts((2, 4, 4))
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # calculate maximum absolute error
    diff = tc['TCOMP3'] - tc0[:, 1]
    diffmax = np.abs(diff).max()
    msg = 'maximum absolute total-compaction difference ({}) '.format(diffmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.comp.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    for i in range(diff.shape[0]):
        line = '{:10.2g}'.format(tc0[i, 0])
        line += '{:10.2g}'.format(tc['TCOMP3'][i])
        line += '{:10.2g}'.format(tc0[i, 1])
        line += '{:10.2g}'.format(diff[i])
        f.write(line + '\n')
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    # get results from listing file
    fpth = os.path.join(sim.simpath,
                        '{}.lst'.format(os.path.basename(sim.name)))
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]

    # get results from cbc file
    cbc_bud = ['CSUB-CGELASTIC', 'CSUB-WATERCOMP']
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.
    fpth = os.path.join(sim.simpath,
                        '{}.cbc'.format(os.path.basename(sim.name)))
    cobj = flopy.utils.CellBudgetFile(fpth, precision='double')
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.
            qout = 0.
            v = cobj.get_data(kstpkper=k, text=text)[0]
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

    if diffmax > dtol:
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
        sim = Simulation(dir, exfunc=eval_comp,
                         exe_dict=replace_exe, htol=htol[idx], idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
