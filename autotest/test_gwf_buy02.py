# Simple one-layer unconfined model with variable density flow. Cases include:
# a. density = denseref = 1000.
# b. density = 1024.5
# c. density = 1024.5, sloping bottom
# d. density variable, flat bottom
# e. density variable, sloping bottom
# f. density variable, sloping bottom, hhformulation_rhs

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

ex = ['buy_02a', 'buy_02b', 'buy_02c', 'buy_02d', 'buy_02e', 'buy_02f']
concbuy = [0., 35., 35.,
           [35., 30., 17.5, 5., 0.],
           [35., 30., 17.5, 5., 0.],
           [35., 30., 17.5, 5., 0.]]
dz = [0., 0., 10., 0., 10., 10.]
hhformulation_rhs = [False, False, False, False, False, True]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))


def get_model(idx, dir):
    lx = 5.
    lz = 1.
    nlay = 1
    nrow = 1
    ncol = 5
    nper = 1
    delc = 1.
    delr = lx / ncol
    ddz = dz[idx]
    top = 100.
    botm = 0.
    if ddz > 0.:
        top = np.arange(top, (top - 5 * ddz), -ddz)
        botm = np.arange(botm, (botm - 5 * ddz), -ddz)

    perlen = [1.0]
    nstp = [1]
    tsmult = [1.]

    Kh = 1.

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    nouter, ninner = 700, 2
    hclose, rclose, relax = 1e-8, 1e-6, 0.97

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
    gwfname = 'gwf_' + name

    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname,
                               print_flows=True,
                               save_flows=True)

    imsgwf = flopy.mf6.ModflowIms(sim, print_option='ALL',
                                  outer_dvclose=hclose,
                                  outer_maximum=nouter,
                                  under_relaxation='NONE',
                                  inner_maximum=ninner,
                                  inner_dvclose=hclose,
                                  rcloserecord=[(rclose, 'strict')],
                                  linear_acceleration='BICGSTAB',
                                  scaling_method='NONE',
                                  reordering_method='NONE',
                                  relaxation_factor=relax,
                                  filename='{}.ims'.format(gwfname))

    idomain = np.full((nlay, nrow, ncol), 1)
    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm, idomain=idomain)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=top)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, xt3doptions=False,
                                  save_flows=True,
                                  save_specific_discharge=True,
                                  icelltype=1, k=Kh)

    d = concbuy[idx]
    hhrhs = hhformulation_rhs[idx]
    pd = [(0, 0.7, 0., 'none', 'none')]
    buy = flopy.mf6.ModflowGwfbuy(gwf, nrhospecies=len(pd), packagedata=pd,
                                  denseref=1000., concentration=d,
                                  hhformulation_rhs=hhrhs)

    # chd files
    chdlist1 = []
    chdlist1.append([(0, 0, 0), 100.])
    chdlist1.append([(0, 0, ncol - 1), 50.])
    chd1 = flopy.mf6.ModflowGwfchd(gwf,
                                   stress_period_data=chdlist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='CHD-1')

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.bud'.format(gwfname),
                                head_filerecord='{}.hds'.format(gwfname),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'LAST')])

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def eval_results(sim):
    print('evaluating results...')

    # read heads
    idx = sim.idxsim
    name = ex[idx]
    gwfname = 'gwf_' + name
    fname = gwfname + '.hds'
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    hobj = flopy.utils.HeadFile(fname, precision='double')
    head = hobj.get_data().flatten()
    print(head)

    # read budget
    fname = gwfname + '.bud'
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.CellBudgetFile(fname, precision='double')
    chdflows = bobj.get_data(text='CHD')[0]
    n, n, q = chdflows[0]

    # calculate the flow between cell 1 and 2 using the simulated heads and
    # the variable density flow equation, and then compare that flow with
    # the flow rate simulated by the model
    h1 = head[0]
    h2 = head[1]
    dh = h1 - h2
    c = concbuy[idx]
    if isinstance(c, list):
        rho1 = 1000. + .7 * c[0]
        rho2 = 1000. + .7 * c[1]
    else:
        rho1 = rho2 = 1000. + 0.7 * c
    z1 = (h1 - 0.) / 2
    ddz = dz[idx]
    b2 = 0. - ddz
    z2 = b2 + (h2 - b2) / 2
    dl = 1.
    k = 1.
    c1 = k * (h1 * 1) / (dl * .5)
    c2 = k * ((h2 - b2) * 1) / (dl * .5)
    condref = c1 * c2 / (c1 + c2)
    denseref = 1000.
    avgdense = .5 * rho1 + .5 * rho2
    avghead = .5 * h1 + .5 * h2
    avgelev = .5 * z1 + .5 * z2
    print(condref, avgdense, avghead, avgelev)

    qans = dh
    qans += (avgdense / denseref - 1.) * dh
    qans += (avghead - avgelev) * (rho1 - rho2) / denseref
    qans *= condref

    print('qans = {}'.format(qans))

    assert np.allclose(q, qans), 'flows not right {} /= {}'.format(q, qans)

    # comment when done testing
    #assert False

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_results, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_results, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
