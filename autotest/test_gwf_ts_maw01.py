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

paktest = 'maw'
ex = ['ts_{}01'.format(paktest)]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))

# run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None


def build_model(ws, name, timeseries=False):
    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((1., 1, 1.0))
    ts_times = np.arange(0., 2., 1., dtype=float)

    auxnames = ['temp', 'conc']
    temp, conc = 32.5, 0.1

    # spatial discretization data
    nlay, nrow, ncol = 3, 10, 10
    delr, delc = 100., 100.
    top = 0.
    botm = [-10, -20, -30]
    strt = 0.

    # calculate hk
    hk = 1.e-4

    # solver options
    nouter, ninner = 1000, 100
    hclose, rclose, relax = 1e-6, 1e-3, 1.
    newtonoptions = ''
    imsla = 'BICGSTAB'

    # build MODFLOW 6 files
    sim = flopy.mf6.MFSimulation(sim_name=name,
                                 memory_print_option='all',
                                 version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)
    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim,
                               print_option='NONE',
                               outer_dvclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='NONE',
                               inner_maximum=ninner,
                               inner_dvclose=hclose, rcloserecord=rclose,
                               linear_acceleration=imsla,
                               scaling_method='NONE',
                               reordering_method='NONE',
                               relaxation_factor=relax)

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name,
                               newtonoptions=newtonoptions,
                               save_flows=True, print_flows=True)

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
    cnvgpth = '{}.sfr.cnvg.csv'.format(name)
    sfr = flopy.mf6.ModflowGwfsfr(gwf,
                                  print_input=True,
                                  mover=True,
                                  nreaches=5,
                                  maximum_depth_change=1.e-5,
                                  package_convergence_filerecord=cnvgpth,
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  perioddata=perioddata, pname='sfr-1')

    packagedata = [[0, 1.0, -20., 0.0, 'SPECIFIED', 2, temp, conc], ]
    nmawwells = len(packagedata)
    connectiondata = [
        [1 - 1, 1 - 1, (1 - 1, 5 - 1, 8 - 1), 0.0, -20, 1.0, 1.1],
        [1 - 1, 2 - 1, (2 - 1, 5 - 1, 8 - 1), 0.0, -20, 1.0, 1.1]]

    perioddata = [[0, 'FLOWING_WELL', 0., 1., 0.1]]
    rate = 4e-3
    ts_names = ['rate'] + auxnames
    if timeseries:
        perioddata.append([0, 'rate', 'rate'])
        perioddata.append([0, 'AUXILIARY', 'conc', 'conc'])
        perioddata.append([0, 'AUXILIARY', 'temp', 'temp'])
        ts_methods = ['linearend'] * len(ts_names)
        ts_data = []
        for t in ts_times:
            ts_data.append((t, rate, temp, conc))
    else:
        perioddata.append([0, 'rate', rate])
        perioddata.append([0, 'AUXILIARY', 'conc', conc])
        perioddata.append([0, 'AUXILIARY', 'temp', temp])

    budpth = '{}.{}.cbc'.format(name, paktest)
    maw = flopy.mf6.ModflowGwfmaw(gwf,
                                  print_head=True,
                                  budget_filerecord=budpth,
                                  mover=True,
                                  auxiliary=auxnames,
                                  flowing_wells=True,
                                  nmawwells=nmawwells,
                                  print_input=True,
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  perioddata=perioddata, pname='maw-1')
    if timeseries:
        fname = '{}.{}.ts'.format(name, paktest)
        maw.ts.initialize(filename=fname, timeseries=ts_data,
                          time_series_namerecord=ts_names,
                          interpolation_methodrecord=ts_methods)

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
    perioddata = [(1, 'status', 'active'),
                  (1, 'rainfall', '0.0'),
                  (1, 'evaporation', '0.0'),
                  (1, 'runoff', '0.0'),
                  (1, 'withdrawal', '0.0'),
                  (0, 'rate', '1.0'),
                  (0, 'invert', '1.0e-003'),
                  (0, 'width', '0.0'),
                  (0, 'slope', '1.0e-003'),
                  (0, 'rough', '1.0e-001')]
    cnvgpth = '{}.lak.cnvg.csv'.format(name)
    lak = flopy.mf6.ModflowGwflak(gwf,
                                  print_input=True,
                                  mover=True,
                                  nlakes=nlakes,
                                  noutlets=noutlets,
                                  print_stage=True,
                                  print_flows=True,
                                  package_convergence_filerecord=cnvgpth,
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  outlets=outlets,
                                  perioddata=perioddata,
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

    cnvgpth = '{}.uzf.cnvg.csv'.format(name)
    uzf = flopy.mf6.ModflowGwfuzf(gwf,
                                  print_input=True,
                                  mover=True,
                                  package_convergence_filerecord=cnvgpth,
                                  nuzfcells=len(packagedata),
                                  ntrailwaves=7, nwavesets=40,
                                  packagedata=packagedata,
                                  perioddata=perioddata, pname='uzf-1')

    packages = [('drn-1',), ('lak-1',), ('maw-1',), ('sfr-1',), ('uzf-1',)]
    perioddata = [
        ('drn-1', 0, 'lak-1', 1, 'excess', 1.),
        ('drn-1', 0, 'maw-1', 0, 'threshold', 1.e-3),
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
        ('uzf-1', 8, 'sfr-1', 0, 'factor', 1.),
        ('maw-1', 0, 'lak-1', 1, 'factor', 0.5)]
    mvr = flopy.mf6.ModflowGwfmvr(gwf, maxmvr=len(perioddata),
                                  budget_filerecord='{}.mvr.bud'.format(name),
                                  maxpackages=len(packages),
                                  print_flows=True,
                                  packages=packages,
                                  perioddata=perioddata)

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('BUDGET', 'LAST')])

    return sim


def get_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = build_model(ws, name)

    # build MODFLOW 6 files with timeseries
    ws = os.path.join(dir, 'mf6')
    mc = build_model(ws, name, timeseries=True)

    return sim, mc


def eval_model(sim):
    print('evaluating model budgets...')

    fname = '{}.cbc'.format(os.path.basename(sim.name))

    # open first gwf cbc file
    fpth = os.path.join(sim.simpath, fname)
    cobj0 = flopy.utils.CellBudgetFile(fpth, precision='double')

    # open second gwf cbc file
    fpth = os.path.join(sim.simpath, 'mf6', fname)
    cobj1 = flopy.utils.CellBudgetFile(fpth, precision='double')

    # define file path and evaluate difference
    fname = '{}.cbc.cmp.out'.format(os.path.basename(sim.name))
    fpth = os.path.join(sim.simpath, fname)
    eval_bud_diff(fpth, cobj0, cobj1)

    # evaluate the sfr package budget file
    fname = '{}.{}.cbc'.format(os.path.basename(sim.name), paktest)
    # open first sfr cbc file
    fpth = os.path.join(sim.simpath, fname)
    cobj0 = flopy.utils.CellBudgetFile(fpth, precision='double')

    # open second sfr cbc file
    fpth = os.path.join(sim.simpath, 'mf6', fname)
    cobj1 = flopy.utils.CellBudgetFile(fpth, precision='double')

    # define file path and evaluate difference
    fname = '{}.{}.cbc.cmp.out'.format(os.path.basename(sim.name), paktest)
    fpth = os.path.join(sim.simpath, fname)
    eval_bud_diff(fpth, cobj0, cobj1)

    return


def eval_bud_diff(fpth, b0, b1, dtol=1e-6):
    diffmax = 0.
    difftag = 'None'
    difftime = None
    fail = False

    # build list of cbc data to retrieve
    avail = b0.get_unique_record_names()

    # initialize list for storing totals for each budget term terms
    cbc_keys = []
    for t in avail:
        if isinstance(t, bytes):
            t = t.decode()
        t = t.strip()
        cbc_keys.append(t)

    # open a summary file and write header
    f = open(fpth, 'w')
    line = '{:15s}'.format('Time')
    line += ' {:15s}'.format('Datatype')
    line += ' {:15s}'.format('Variables')
    line += ' {:15s}'.format('Timeseries')
    line += ' {:15s}'.format('Difference')
    f.write(line + '\n')
    f.write(len(line) * '-' + '\n')

    # get data from cbc file
    kk = b0.get_kstpkper()
    times = b0.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        v0sum = 0.
        v1sum = 0.
        for key in cbc_keys:
            v0 = b0.get_data(kstpkper=k, text=key)[0]
            v1 = b1.get_data(kstpkper=k, text=key)[0]
            if isinstance(v0, np.recarray):
                v0 = v0['q'].sum()
                v1 = v1['q'].sum()
            else:
                v0 = v0.flatten().sum()
                v1 = v1.flatten().sum()

            # sum all of the values
            if key != 'AUXILIARY':
                v0sum += v0
                v1sum += v1

            diff = v0 - v1
            if abs(diff) > abs(diffmax):
                diffmax = diff
                difftag = key
                difftime = t
            if abs(diff) > dtol:
                fail = True
            line = '{:15g}'.format(t)
            line += ' {:15s}'.format(key)
            line += ' {:15g}'.format(v0)
            line += ' {:15g}'.format(v1)
            line += ' {:15g}'.format(diff)
            f.write(line + '\n')

    # evaluate the sums
    diff = v0sum - v1sum
    if abs(diff) > dtol:
        fail = True
    line = '{:15g}'.format(t)
    line += ' {:15s}'.format('TOTAL')
    line += ' {:15g}'.format(v0sum)
    line += ' {:15g}'.format(v1sum)
    line += ' {:15g}'.format(diff)
    f.write(line + '\n')

    msg = '\nSummary of changes in {}\n'.format(os.path.basename(fpth))
    msg += '-' * 72 + '\n'
    msg += 'Maximum cbc difference:        {}\n'.format(diffmax)
    msg += 'Maximum cbc difference time:   {}\n'.format(difftime)
    msg += 'Maximum cbc datatype:          {}\n'.format(difftag)
    if fail:
        msg += 'Maximum cbc criteria exceeded:  {}'.format(dtol)
    assert not fail, msg

    # close summary file and print the final message
    f.close()
    print(msg)

    msg = 'sum of first cbc file flows ({}) '.format(v0sum) + \
          'exceeds dtol ({})'.format(dtol)
    assert abs(v0sum) < dtol, msg

    msg = 'sum of second cbc file flows ({}) '.format(v1sum) + \
          'exceeds dtol ({})'.format(dtol)
    assert abs(v1sum) < dtol, msg

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_simulation()
    return


def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_model, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_model, idxsim=idx)
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
