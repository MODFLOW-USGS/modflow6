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

from framework import testing_framework
from simulation import Simulation

ex = ['maw01', 'maw01nwt', 'maw01nwtur']
newtonoptions = [None, [''], ['UNDER_RELAXATION']]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def build_models():

    nlay, nrow, ncol = 1, 1, 3
    nper = 3
    perlen = [1., 1., 1.]
    nstp = [1, 1, 1]
    tsmult = [1., 1., 1.]
    steady = [True, True, True]
    lenx = 300.
    delr = delc = lenx / float(nrow)
    botm = [0.]
    strt = 100.
    hnoflo = 1e30
    hdry = -1e30
    hk = 1.

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 0.01, 1.

    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

    for idx, dir in enumerate(exdirs):
        name = ex[idx]

        # build MODFLOW 6 files
        ws = dir
        sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                     exe_name='mf6',
                                     sim_ws=ws,
                                     sim_tdis_file='simulation.tdis')
        # create tdis package
        tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                     nper=nper, tdisrecarray=tdis_rc)

        # create gwf model
        gwf = flopy.mf6.MFModel(sim, model_type='gwf6', model_name=name,
                                model_nam_file='{}.nam'.format(name),
                                ims_file_name='{}.ims'.format(name))
        gwf.name_file.newtonoptions = newtonoptions[idx]

        # create iterative model solution and register the gwf model with it
        ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                                   outer_hclose=hclose,
                                   outer_maximum=nouter,
                                   under_relaxation='NONE',
                                   inner_maximum=ninner,
                                   inner_hclose=hclose, rcloserecord=rclose,
                                   linear_acceleration='BICGSTAB',
                                   scaling_method='NONE',
                                   reordering_method='NONE',
                                   relaxation_factor=relax)
        sim.register_ims_package(ims, [gwf.name])

        dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                      delr=delr, delc=delc,
                                      top=100., botm=0.,
                                      idomain=1,
                                      fname='{}.dis'.format(name))

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                    fname='{}.ic'.format(name))

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True,
                                      icelltype=1,
                                      k=hk,
                                      k33=hk,
                                      fname='{}.npf'.format(name))
        # storage
        sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=True,
                                      iconvert=1,
                                      ss=0., sy=0.1,
                                      steady_state={0: True},
                                      transient={1: False},
                                      fname='{}.sto'.format(name))

        # chd files
        chdlist0 = []
        chdlist0.append([(0, 0, 0), 100.])
        chdlist0.append([(0, 0, 2), 100.])

        chdlist1 = []
        chdlist1.append([(0, 0, 0), 25.])
        chdlist1.append([(0, 0, 2), 25.])

        chdspdict = {0: chdlist0, 1: chdlist1, 2: chdlist0}
        chd = flopy.mf6.ModflowGwfchd(gwf,
                                      periodrecarray=chdspdict,
                                      save_flows=False,
                                      fname='{}.chd'.format(name))

        # wel files
        #wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
        #                              maxbound=len(ws),
        #                              periodrecarray=wd6,
        #                              save_flows=False)
        # MAW
        wellbottom = 50.
        wellrecarray = [[0 + 1, 0.1, wellbottom, 100., 'THEIM', 1]]
        wellconnectionsrecarray = [[0 + 1, 0 + 1, (0, 0, 1), 100., wellbottom, 1., 0.1]]
        wellperiodrecarray = [[0 + 1, 'rate', 0.]]
        maw = flopy.mf6.ModflowGwfmaw(gwf, fname='{}.maw'.format(name),
                                      print_input=True, print_head=True,
                                      print_flows=True, save_flows=True,
                                      wellrecarray=wellrecarray,
                                      wellconnectionsrecarray=wellconnectionsrecarray,
                                      wellperiodrecarray=wellperiodrecarray)

        # output control
        oc = flopy.mf6.ModflowGwfoc(gwf,
                                    budget_filerecord='{}.cbc'.format(name),
                                    head_filerecord='{}.hds'.format(name),
                                    headprintrecord=[
                                        ('COLUMNS', 10, 'WIDTH', 15,
                                         'DIGITS', 6, 'GENERAL')],
                                    saverecord=[('HEAD', 'ALL')],
                                    printrecord=[('HEAD', 'ALL'),
                                                 ('BUDGET', 'ALL')],
                                    fname='{}.oc'.format(name))

        # write MODFLOW 6 files
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
