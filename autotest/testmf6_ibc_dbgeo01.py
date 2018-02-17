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

from framework import testing_framework
from simulation import Simulation

ex = ['ibcdbgeo01a']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'

ndcell = [21, 21]
strt = [10., -100.]
chdh = [5, -100.]
gso = [True, None]
bso = [True, None]

# run all examples on Travis
# travis = [True for idx in range(len(exdirs))]
# the delay bed problems only run on the development version of MODFLOW-2005
# set travis to True when version 1.13.0 is released
travis = [False for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = {'mf2005': 'mf2005devdbl'}


def build_models():
    nlay, nrow, ncol = 1, 1, 3
    nper = 1
    perlen = [1000. for i in range(nper)]
    nstp = [100 for i in range(nper)]
    tsmult = [1.05 for i in range(nper)]
    steady = [False for i in range(nper)]
    delr, delc = 1., 1.
    top = 0.
    botm = [-100.]
    hnoflo = 1e30
    hdry = -1e30
    hk = 1e6
    laytyp = [0]
    ss = 0.
    sy = 0.2

    nouter, ninner = 1000, 300
    hclose, rclose, relax = 1e-6, 1e-6, 0.97

    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

    ib = 1

    # sub data
    cc = 0.25
    cr = 0.01
    void = 0.82
    theta = void / (1. + void)
    kv = 1e-3
    sgm = 1.7
    sgs = 2.2
    ini_stress = 0.0
    thick = [10.]

    sub6 = [[1, (0, 0, 1), 'delay', ini_stress, thick[0],
             1., cc, cr, void, kv, ini_stress]]

    for idx, dir in enumerate(exdirs):

        c6 = []
        for j in range(0, ncol, 2):
            c6.append([(0, 0, j), chdh[idx]])
        cd6 = {0: c6}


        name = ex[idx]

        # build MODFLOW 6 files
        ws = dir
        sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                     exe_name='mf6',
                                     sim_ws=ws,
                                     sim_tdis_file='simulation.tdis')
        # create tdis package
        tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                     nper=nper, perioddata=tdis_rc)

        # create gwf model
        gwf = flopy.mf6.MFModel(sim, model_type='gwf6', modelname=name,
                                model_nam_file='{}.nam'.format(name))

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
        opth = '{}.ibc.obs'.format(name)
        ibc = flopy.mf6.ModflowGwfibc(gwf, ndelaycells=ndcell[idx],
                                      compression_indices=True,
                                      geo_stress_offset=gso[idx],
                                      interbed_stress_offset=bso[idx],
                                      obs_filerecord=opth,
                                      nibccells=1,
                                      sgs=sgs, sgm=sgm, packagedata=sub6)
        orecarray = {}
        orecarray['ibc_obs.csv'] = [('tcomp', 'total-compaction', (0, 0, 1)),
                                    ('gs', 'gstress-cell', (0, 0, 1)),
                                    ('es', 'estress-cell', (0, 0, 1)),
                                    ('pcs', 'preconstress', (0, 0))]
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

    return


def eval_sub(sim):
    print('evaluating subsidence...')

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, 'ibc_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # MODFLOW-2005 total compaction results
    fpth = os.path.join(sim.simpath, 'mf2005',
                        '{}.total_comp.hds'.format(ex[sim.idxsim]))
    try:
        sobj = flopy.utils.HeadFile(fpth, text='LAYER COMPACTION')
        tc0 = sobj.get_ts((0, 0, 1))
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # calculate maximum absolute error
    diff = tc['TCOMP'] - tc0[:, 1]
    diffmax = np.abs(diff).max()
    dtol = 1e-6
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


# use python testmf6_ibc_sub01.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
