import os
import numpy as np

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = ['csub_dbgeo01a']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'

ndcell = [19]
strt = [0.]
chdh = [0]
gso = [True]
bso = [True]
# sstate = [None, True]

# run all examples on Travis
# travis = [True for idx in range(len(exdirs))]
# the delay bed problems only run on the development version of MODFLOW-2005
# set travis to True when version 1.13.0 is released
travis = [False for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = {'mf2005': 'mf2005devdbl'}

# comparison data
compdataa = [0.68230401, 1.3532073, 2.01244404, 2.65989315, 3.29557902,
             3.91966976, 4.53247275, 5.13442771, 5.72609742, 6.3081564,
             6.88137778, 7.44661891, 8.004806, 8.55691833, 9.10397262,
             9.64700778, 10.18707071, 10.7252034, 11.26243157, 11.79975525,
             12.33814117, 12.87851721, 13.42176872, 13.96873661, 14.52021703,
             15.07696238, 15.63968342, 16.20905225, 16.78570572, 17.37024932,
             17.96326112, 18.56529569, 19.17688787, 19.79855626, 20.43080648,
             21.074134, 21.72902678, 22.39596747, 23.07543537, 23.76790816,
             24.47386341, 25.19377979, 25.92813826, 26.67742297, 27.44212214,
             28.22272872, 29.01974092, 29.83366264, 30.66500362, 31.51427945,
             32.38201122, 33.26872482, 34.17494985, 35.10121781, 36.04805978,
             37.01600319, 38.00556759, 39.01725945, 40.05156568, 41.10894582,
             42.18982291, 43.29457289, 44.4235127, 45.57688697, 46.75485359,
             47.95746837, 49.18466885, 50.43625788, 51.71188703, 53.01104041,
             54.33301926, 55.67692765, 57.04165979, 58.42588923, 59.82806042,
             61.24638278, 62.67882766, 64.12312837, 65.57678339, 67.037063,
             68.50101935, 69.96549998, 71.427165, 72.88250769, 74.3278787,
             75.75951364, 77.17356409, 78.56613174, 79.93330548, 81.27120122,
             82.57600395, 83.84401149, 85.07167958, 86.25566726, 87.39288195,
             88.48052328, 89.51612453, 90.49759079, 91.42323263, 92.29179429]

compdata = {0: compdataa}

# static model data
# spatial discretization
nlay, nrow, ncol = 1, 1, 3
delr, delc = 1., 1.
top = 0.
bots = [-100.]
botm = [top] + bots

# temporal discretization
nper = 1
perlen = [1000. for i in range(nper)]
nstp = [100 for i in range(nper)]
tsmult = [1.05 for i in range(nper)]
steady = [False for i in range(nper)]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

hnoflo = 1e30
hdry = -1e30

# idomain data
ib = 1

# npf and sto data
hk = 1e6
laytyp = [1]
ss = 0.
sy = 0.2

# solver data
nouter, ninner = 50, 100
hclose, rclose, relax = 1e-6, 1e-6, 0.97

# sub data
cc = 99.161639  # 100.
cr = 0.99161639  # 1.
void = 0.82
theta = void / (1. + void)
kv = 0.025
sgm = 1.7
sgs = 2.2
ini_stress = 1.0
thick = [1., 10.]


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


def get_model(idx, dir):
    c6 = []
    for j in range(0, ncol, 2):
        c6.append([(0, 0, j), chdh[idx]])
    cd6 = {0: c6}

    geo, es = calc_stress(sgm, sgs, strt[idx], botm)
    sub6 = [[1, (0, 0, 1), 'delay', -1., thick[0],
             1., cc, cr, theta, kv, 1.]]

    name = ex[idx]

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
                                  top=top, botm=bots,
                                  filename='{}.dis'.format(name))

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt[idx],
                                filename='{}.ic'.format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=laytyp,
                                  k=hk,
                                  k33=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False, iconvert=laytyp,
                                  ss=ss, sy=sy,
                                  storagecoefficient=True,
                                  transient={0: True})

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                   maxbound=len(c6),
                                                   stress_period_data=cd6,
                                                   save_flows=False)

    # csub files
    opth = '{}.csub.obs'.format(name)
    ibcsv = '{}.ib.strain.csv'.format(name)
    skcsv = '{}.sk.strain.csv'.format(name)
    csub = flopy.mf6.ModflowGwfcsub(gwf, ndelaycells=ndcell[idx],
                                    strainib_filerecord=ibcsv,
                                    strainsk_filerecord=skcsv,
                                    time_weight=0.,
                                    # compression_indices=True,
                                    ninterbeds=1,
                                    sgs=sgs, sgm=sgm, packagedata=sub6,
                                    beta=0., ske_cr=0.)
    orecarray = {}
    orecarray['csub_obs.csv'] = [('tcomp', 'interbed-compaction', (0, 0, 1)),
                                 ('gs', 'gstress-cell', (0, 0, 1)),
                                 ('es', 'estress-cell', (0, 0, 1)),
                                 ('pcs', 'preconstress', (0, 0)),
                                 ('sk', 'sk', (0, 0, 1))]
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
                                saverecord=[('HEAD', 'ALL')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'ALL')])

    mc = None

    return sim, mc


def eval_sub(sim):
    print('evaluating subsidence...')

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, 'csub_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # set comparison data
    tc0 = compdata[sim.idxsim]

    # calculate maximum absolute error
    diff = tc['TCOMP'] - tc0[:]
    diffmax = np.abs(diff).max()
    dtol = 1e-6
    msg = 'maximum absolute total-compaction difference ({}) '.format(diffmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.comp.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    line = '{:>15s}'.format('TOTIM')
    line += ' {:>15s}'.format('CSUB')
    line += ' {:>15s}'.format('MF')
    line += ' {:>15s}'.format('DIFF')
    f.write(line + '\n')
    for i in range(diff.shape[0]):
        line = '{:15g}'.format(tc0[i])
        line += ' {:15g}'.format(tc['TCOMP'][i])
        line += ' {:15g}'.format(tc0[i])
        line += ' {:15g}'.format(diff[i])
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
        yield test.run_mf6, Simulation(dir, exfunc=eval_sub,
                                       exe_dict=r_exe, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_sub, exe_dict=replace_exe,
                         idxsim=idx)
        test.run_mf6(sim)
    return


# use python testmf6_csub_sub01.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
