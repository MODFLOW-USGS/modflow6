import os
import sys
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

ex = ['ats01',]
top = [100.]
laytyp = [1]
ss = [1.e-4]
sy = [0.1]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def get_model(idx, dir):
    nlay, nrow, ncol = 1, 75, 75
    nper = 3
    perlen = [1., 100., 100.]
    nstp = [1, 10, 10]
    tsmult = [1., 1.3, 1.3]
    lenx = 20000.
    delr = delc = lenx / float(nrow)
    botm = [-100.]
    strt = 40.
    hnoflo = 1e30
    hdry = -1e30
    mu = 5.
    sigma = 1.23
    np.random.seed(seed=9001)
    hk = 1.0 #np.random.lognormal(mu, sigma, (nrow, ncol))

    nc = int((nrow - 1) / 2) + 1
    welsp = [((0, nc, nc), -150000.)]
    welspd = {1: welsp, 2:[[]]}

    c = []
    c6 = []
    for i in range(nrow):
        c.append([0, i, 0, 48., 48.])
        c.append([0, i, ncol - 1, 40., 40.])
        c6.append([(0, i, 0), 48.])
        c6.append([(0, i, ncol - 1), 40.])
    cd = {0: c}
    cd6 = {0: c6}

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(sim_name=name,
                                 version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    atsperiod = [(1, 100,  1.e-5, 100),
                 (2, 0.01, 1.e-5, 10),]
    ats = flopy.mf6.ModflowUtlats(sim,
                                  maxats=len(atsperiod),
                                  perioddata=atsperiod)

    tdis = flopy.mf6.ModflowTdis(sim,
                                 ats_filerecord="ats01.ats",
                                 time_units='DAYS',
                                 nper=nper,
                                 perioddata=tdis_rc)

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, newtonoptions=True)

    # create iterative model solution and register the gwf model with it
    nouter, ninner = 20, 100  # was 20 25
    hclose, rclose, relax = 1e-6, 0.01, 1.
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
                                  top=top[idx], botm=botm,
                                  idomain=1)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=laytyp[idx],
                                  k=hk,
                                  k33=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False,
                                  iconvert=laytyp[idx],
                                  ss=ss[idx], sy=sy[idx],
                                  steady_state={0: True, 1: False},
                                  transient={0: False, 1: True})

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                   maxbound=len(c6),
                                                   stress_period_data=cd6,
                                                   save_flows=False)

    # wel files
    wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True,
                                  print_flows=True,
                                  maxbound=len(welsp),
                                  stress_period_data=welspd,
                                  save_flows=False,
                                  auto_flow_reduce=False)

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'ALL')])

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for dir in exdirs:
        yield test.run_mf6, Simulation(dir)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for dir in exdirs:
        sim = Simulation(dir)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
