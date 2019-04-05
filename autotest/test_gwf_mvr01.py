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
    name = 'gwf_mvr01'
    mf6_exe = os.path.abspath(targets.target_dict['mf6'])

    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((1., 1, 1.0))

    # spatial discretization data
    nlay, nrow, ncol = 3, 10, 10
    delr, delc = 100., 100.
    top = 0.
    botm = [-10, -20, -30]
    strt = 0.

    # calculate hk
    hk = 1.e-4

    # solver options
    nouter, ninner = 400, 100
    hclose, rclose, relax = 1e-4, 0.1, 1.
    newtonoptions = ''
    imsla = 'BICGSTAB'

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
                               save_flows=True, print_flows=True)

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
                                  top=top, botm=botm)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=hk)

    # chd files
    # chd data
    spd = [[(0, 0, 0), 1.],
           [(0, nrow - 1, ncol - 1), 0.], ]
    chd = flopy.mf6.modflow.ModflowGwfchd(gwf, stress_period_data=spd,
                                          pname='chd-1')

    # drn file
    drn6 = [[(0, 1, 2), -1., 1.],
            [(0, 2, 3), -1., 1.], ]
    drn = flopy.mf6.modflow.ModflowGwfdrn(gwf, mover=True,
                                          stress_period_data=drn6,
                                          pname='drn-1')

    # sfr file
    packagedata = [
        [0, (1 - 1, 4 - 1, 1 - 1), 3.628E+001, 1.0, 1.0E-003, 0.0, 1.0, 1.0E-4,
         1.0E-1, 1, 0.0, 0],
        [1, (1 - 1, 4 - 1, 2 - 1), 1.061E+002, 1.0, 1.0E-003, 0.0, 1.0, 1.0E-4,
         1.0E-1, 2, 1.0, 0],
        [2, (1 - 1, 4 - 1, 3 - 1), 6.333E+001, 1.0, 1.0E-003, 0.0, 1.0, 1.0E-4,
         1.0E-1, 2, 1.0, 0],
        [3, (1 - 1, 5 - 1, 3 - 1), 4.279E+001, 1.0, 1.0E-003, 0.0, 1.0, 1.0E-4,
         1.0E-1, 2, 1.0, 0],
        [4, (1 - 1, 5 - 1, 4 - 1), 6.532E+001, 1.0, 1.0E-003, 0.0, 1.0, 1.0E-4,
         1.0E-1, 1, 1.0, 0],
    ]
    connectiondata = [
        [0, -1],
        [1, 0, -2],
        [2, 1, -3],
        [3, 2, -4],
        [4, 3],
    ]
    perioddata = [
        [0, 'status', 'active'],
        [1, 'status', 'active'],
        [2, 'status', 'active'],
        [3, 'status', 'active'],
        [4, 'status', 'active'],
    ]
    sfr = flopy.mf6.ModflowGwfsfr(gwf, mover=True, nreaches=5,
                                  maximum_depth_change=1.e-5,
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  perioddata=perioddata, pname='sfr-1')

    packagedata = [[0, 1.0, -20., 0.0, 'SPECIFIED', 2], ]
    nmawwells = len(packagedata)
    connectiondata = [[1 - 1, 1 - 1, (1 - 1, 5 - 1, 8 - 1), 0.0, -20, 1.0, 1.1],
                      [1 - 1, 2 - 1, (2 - 1, 5 - 1, 8 - 1), 0.0, -20, 1.0, 1.1]]
    perioddata = [[0, 'FLOWING_WELL', 0., 0.],
                  [0, 'RATE', 1.e-3]]
    maw = flopy.mf6.ModflowGwfmaw(gwf, mover=True, nmawwells=nmawwells,
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  perioddata=perioddata, pname='maw-1')

    packagedata = [(0, 1., 11),
                   (1, 0.5, 11)]
    outlets = [(0, 0, 1, 'manning', 0.001, 0., 0.1, 0.001)]
    nlakes = len(packagedata)
    noutlets = len(outlets)
    connectiondata = [
        (0, 0, (0, 0, 5), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 1, (0, 1, 4), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 2, (1, 1, 5), 'vertical', 1.e-05, -5., 0., 1., 0.),
        (0, 3, (0, 2, 4), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 4, (0, 3, 5), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 5, (0, 2, 6), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 6, (1, 2, 5), 'vertical', 1.e-05, -5., 0., 1., 0.),
        (0, 7, (0, 0, 6), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 8, (0, 2, 6), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 9, (0, 1, 7), 'horizontal', 1.e-05, -5., 0., 100., 100.),
        (0, 10, (1, 1, 6), 'vertical', 1.e-05, -5., 0., 1., 0.),
        (1, 0, (0, 0, 8), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 1, (0, 1, 7), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 2, (0, 1, 9), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 3, (1, 1, 8), 'vertical', 1.e-05, -1., 0., 0., 0.),
        (1, 4, (0, 2, 7), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 5, (0, 2, 9), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 6, (1, 2, 8), 'vertical', 1.e-05, -1., 0., 0., 0.),
        (1, 7, (0, 3, 7), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 8, (0, 4, 8), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 9, (0, 3, 9), 'horizontal', 1.e-05, -1., 0., 100., 100.),
        (1, 10, (1, 3, 8), 'vertical', 1.e-05, -1., 0., 0., 0.)]
    lakeperioddata = [(1, 'status', 'active'),
                      (1, 'rainfall', '0.0'),
                      (1, 'evaporation', '0.000000000000e+000'),
                      (1, 'runoff', '0.000000000000e+000'),
                      (1, 'withdrawal', '0.000000000000e+000')]
    outletperioddata = [(0, 'rate', '1.000000000000e+000'),
                        (0, 'invert', '1.000000000000e-003'),
                        (0, 'width', '0.000000000000e+000'),
                        (0, 'slope', '1.000000000000e-003'),
                        (0, 'rough', '1.000000000000e-001')]
    lak = flopy.mf6.ModflowGwflak(gwf, mover=True, nlakes=nlakes,
                                  noutlets=noutlets,
                                  print_stage=True,
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  outlets=outlets,
                                  lakeperioddata=lakeperioddata,
                                  outletperioddata=outletperioddata,
                                  pname='lak-1')

    packagedata = [
        (0, (0, 5, 1), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (1, (0, 5, 2), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (2, (0, 5, 3), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (3, (0, 6, 1), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (4, (0, 6, 2), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (5, (0, 6, 3), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (6, (0, 7, 1), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (7, (0, 7, 2), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5),
        (8, (0, 7, 3), 1, -1, 1., 1.e-05, 0.2, 0.4, 0.3, 3.5)]
    perioddata = [[0, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [1, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [2, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [3, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [4, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [5, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [6, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [7, 1.e-8, 0, 0, 0, 0, 0, 0],
                  [8, 1.e-8, 0, 0, 0, 0, 0, 0], ]
    uzf = flopy.mf6.ModflowGwfuzf(gwf, mover=True, nuzfcells=len(packagedata),
                                  ntrailwaves=7, nwavesets=40,
                                  packagedata=packagedata,
                                  perioddata=perioddata, pname='uzf-1')

    packages = [('drn-1',), ('lak-1',), ('maw-1',), ('sfr-1',), ('uzf-1',)]
    perioddata = [
        ('drn-1', 0, 'lak-1', 1, 'excess', 1.),
        ('drn-1', 0, 'maw-1', 0, 'threshold', 2.),
        ('drn-1', 0, 'sfr-1', 2, 'upto', 3.),
        ('drn-1', 1, 'lak-1', 1, 'excess', 1.),
        ('drn-1', 1, 'maw-1', 0, 'threshold', 2.),
        ('drn-1', 1, 'sfr-1', 2, 'upto', 3.),
        ('lak-1', 0, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 0, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 1, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 2, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 3, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 4, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 5, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 6, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 7, 'sfr-1', 0, 'factor', 1.),
        ('uzf-1', 8, 'sfr-1', 0, 'factor', 1.)]
    mvr = flopy.mf6.ModflowGwfmvr(gwf, maxmvr=len(perioddata),
                                  maxpackages=len(packages),
                                  print_flows=True,
                                  packages=packages,
                                  perioddata=perioddata)

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


def run_mf6():
    sim = get_model()
    sim.write_simulation()
    success, buff = sim.run_simulation()
    msg = 'could not run {}'.format(sim.name)
    assert success, msg
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
