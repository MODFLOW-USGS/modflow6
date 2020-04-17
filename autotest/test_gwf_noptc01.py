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

ex = ['gwf_noptc01', 'gwf_noptc02', 'gwf_noptc03']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))

no_ptcrecords = ['FIRST', 'ALL', '']

ddir = 'data'

## run all examples on Travis
travis = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
# replace_exe = {'mf2005': 'mf2005devdbl'}
replace_exe = None

htol = [None for idx in range(len(exdirs))]

# static model data
# temporal discretization
nper = 1
tdis_rc = [(1., 1, 1.0)]

# spatial discretization data
nlay, nrow, ncol = 1, 1, 100
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 50., 1.
top = 25.
botm = 0.
strt = 0.

# hydraulic properties
hk = 50.

# all cells are active and layer 1 is convertible
ib = 1

# solver options
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.
newtonoptions = ''
imsla = 'BICGSTAB'

# chd data
c6 = []
ccol = [0, ncol - 1]
hc = [20., 11.]
for j, h in zip(ccol, hc):
    c6.append([(0, 0, j), h])
cd6 = {0: c6}
maxchd = len(cd6[0])

# recharge data
rech = {0: 0.001}


def build_model(idx, dir, no_ptcrecord):
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
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name,
                               newtonoptions=newtonoptions,
                               save_flows=True)

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               no_ptcrecord=no_ptcrecord,
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
                                  top=top, botm=botm)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=1,
                                  k=hk)

    # recharge
    rch = flopy.mf6.ModflowGwfrcha(gwf, readasarrays=True, recharge=rech)

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                   maxbound=maxchd,
                                                   stress_period_data=cd6,
                                                   save_flows=False)

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'ALL')])

    return sim


# water table recharge problem
def get_model(idx, dir):
    sim = build_model(idx, dir, no_ptcrecords[idx])

    # build MODFLOW-6 without no_ptc option
    pth = os.path.join(dir, 'mf6')
    mc = build_model(idx, pth, None)

    return sim, mc


def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        mc.write_simulation()
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
        yield test.run_mf6, Simulation(dir, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, idxsim=idx)
        test.run_mf6(sim)

    return


# use python testmf6_csub_sub03.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
