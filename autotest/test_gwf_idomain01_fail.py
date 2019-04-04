import os
from nose.tools import *

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

from framework import testing_framework
import targets


def get_model():
    name = 'csub_idomain01'
    mf6_exe = os.path.abspath(targets.target_dict['mf6'])

    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((1., 1, 1.0))

    # spatial discretization data
    nlay, nrow, ncol = 1, 10, 10
    delr, delc = 1000., 1000.
    top = 0.
    botm = [-100]
    strt = 0.

    # calculate hk
    hk = 1.

    # solver options
    nouter, ninner = 500, 300
    hclose, rclose, relax = 1e-9, 1e-6, 1.
    newtonoptions = ''
    imsla = 'BICGSTAB'

    # chd data
    c6 = []
    ccol = [1, ncol - 1]
    for j in ccol:
        c6.append([(0, nrow - 1, j), strt])
    cd6 = {0: c6}
    maxchd = len(cd6[0])

    # build MODFLOW 6 files
    ws = os.path.join('temp', name)
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name=mf6_exe,
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
                               outer_hclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='NONE',
                               inner_maximum=ninner,
                               inner_hclose=hclose, rcloserecord=rclose,
                               linear_acceleration=imsla,
                               scaling_method='NONE',
                               reordering_method='NONE',
                               relaxation_factor=relax)
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm, idomain=0)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=1,
                                  k=hk)

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


@raises(AssertionError)
def run_mf6():
    sim = get_model()
    success, buff = sim.run_simulation()
    msg = 'could not run {}'.format(sim.name)
    if not success:
        raise AssertionError(msg)
    return


def test_mf6model():
    # run the test models
    yield run_mf6

    return


def main():
    run_mf6()

    return


# use python testmf6_csub_sub03.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
