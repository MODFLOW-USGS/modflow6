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

ndcell = [21, 21, 21]
strt = [0., 10., -100.]
chdh = [0, 5, -100.]
gso = [True, None]
bso = [True, None]

# run all examples on Travis
# travis = [True for idx in range(len(exdirs))]
# the delay bed problems only run on the development version of MODFLOW-2005
# set travis to True when version 1.13.0 is released
travis = [False for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = {'mf2005': 'mf2005devdbl'}

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
cc = 99.161639 #100.
cr = 0.99161639 #1.
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
                                  fname='{}.dis'.format(name))

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt[idx],
                                fname='{}.ic'.format(name))

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

    # ibc files
    opth = '{}.csub.obs'.format(name)
    csub = flopy.mf6.ModflowGwfcsub(gwf, ndelaycells=ndcell[idx],
                                    strain_table='all',
                                    time_weight=0.,
                                    delay_saturation_scaling=True,
                                    #compression_indices=True,
                                    #geo_stress_offset=gso[idx],
                                    interbed_stress_offset=bso[idx],
                                    obs_filerecord=opth,
                                    ninterbeds=1,
                                    sgs=sgs, sgm=sgm, packagedata=sub6,
                                    beta=0., ske_cr=0.)
    orecarray = {}
    orecarray['csub_obs.csv'] = [('tcomp', 'total-compaction', (0, 0, 1)),
                                ('gs', 'gstress-cell', (0, 0, 1)),
                                ('es', 'estress-cell', (0, 0, 1)),
                                ('pcs', 'preconstress', (0, 0)),
                                ('sk', 'sk', (0, 0, 1))]
    ibc_obs_package = flopy.mf6.ModflowUtlobs(gwf,
                                              fname=opth,
                                              parent_file=csub, digits=10,
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

    # # MODFLOW-2005 total compaction results
    # fpth = os.path.join(sim.simpath, 'mf2005',
    #                     '{}.total_comp.hds'.format(ex[sim.idxsim]))
    # try:
    #     sobj = flopy.utils.HeadFile(fpth, text='LAYER COMPACTION')
    #     tc0 = sobj.get_ts((0, 0, 1))
    # except:
    #     assert False, 'could not load data from "{}"'.format(fpth)
    #
    # # calculate maximum absolute error
    # diff = tc['TCOMP'] - tc0[:, 1]
    # diffmax = np.abs(diff).max()
    # dtol = 1e-6
    # msg = 'maximum absolute total-compaction difference ({}) '.format(diffmax)
    #
    # if diffmax > dtol:
    #     sim.success = False
    #     msg += 'exceeds {}'.format(dtol)
    #     assert diffmax < dtol, msg
    # else:
    #     sim.success = True
    #     print('    ' + msg)

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
        sim = Simulation(dir, exfunc=None, exe_dict=replace_exe,
                         idxsim=idx)
        test.run_mf6(sim)
    return


# use python testmf6_csub_sub01.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
