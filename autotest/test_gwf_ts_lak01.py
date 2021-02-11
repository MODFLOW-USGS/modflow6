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

paktest = 'lak'
budtol = 1e-2

ex = ['ts_lak01']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'

# run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
# spatial discretization
nlay, nrow, ncol = 5, 17, 17
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr = delc = [250.00, 1000.0, 1000.0, 1000.0, 1000.0, 1000.0,
               500.00, 500.00, 500.00, 500.00, 500.00, 1000.0,
               1000.0, 1000.0, 1000.0, 1000.0, 250.00]
top = 500.
botm = [107., 97., 87., 77., 67.]
idomain = np.ones(shape3d, dtype=int)
idomain[0, 6:11, 6:11] = 0
idomain[1, 7:10, 7:10] = 0

# temporal discretization
nper = 10
sim_time = 15000.
pertime = sim_time / float(nper)
period_data = []
for n in range(nper):
    period_data.append((pertime, 10, 1.0))

strt = 115.
icelltype = iconvert = 1
kh, kv, sy, ss = 30., [1179., 30., 30., 30.], 3e-4, 0.2
storage_coefficient = True

# chd data
chd_spd = []
chd_arr = np.linspace(160, 140, ncol)
for k in range(nlay):
    for j in range(nrow):
        if j > 0 and j < nrow - 1:
            chd_spd.append([(k, j, 0), chd_arr[0]])
            chd_spd.append([(k, j, ncol - 1), chd_arr[-1]])
        else:
            for i in range(ncol):
                chd_spd.append([(k, j, i), chd_arr[i]])

# recharge data
recharge = 0.116e-1

# lake data
stage, temp, conc = 110., 75., 0.5
packagedata = [(0, stage, 57, temp, conc)]
outlets = [(0, 0, -1, 'SPECIFIED', -999, -999, -999, -999)]
nlakes = len(packagedata)
noutlets = len(outlets)
connectiondata = [
    (0, 0, (0, 6, 5), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 1, (0, 7, 5), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 2, (0, 8, 5), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 3, (0, 9, 5), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 4, (0, 10, 5), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 5, (0, 5, 6), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 6, (1, 6, 6), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 7, (1, 7, 6), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 8, (1, 7, 6), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 9, (1, 8, 6), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 10, (1, 8, 6), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 11, (1, 9, 6), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 12, (1, 9, 6), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 13, (1, 10, 6), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 14, (0, 11, 6), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 15, (0, 5, 7), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 16, (1, 6, 7), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 17, (1, 6, 7), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 18, (2, 7, 7), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 19, (2, 8, 7), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 20, (2, 9, 7), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 21, (1, 10, 7), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 22, (1, 10, 7), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 23, (0, 11, 7), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 24, (0, 5, 8), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 25, (1, 6, 8), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 26, (1, 6, 8), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 27, (2, 7, 8), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 28, (2, 8, 8), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 29, (2, 9, 8), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 30, (1, 10, 8), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 31, (1, 10, 8), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 32, (0, 11, 8), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 33, (0, 5, 9), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 34, (1, 6, 9), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 35, (1, 6, 9), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 36, (2, 7, 9), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 37, (2, 8, 9), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 38, (2, 9, 9), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 39, (1, 10, 9), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 40, (1, 10, 9), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 41, (0, 11, 9), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 42, (0, 5, 10), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 43, (1, 6, 10), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 44, (1, 7, 10), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 45, (1, 7, 10), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 46, (1, 8, 10), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 47, (1, 8, 10), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 48, (1, 9, 10), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 49, (1, 9, 10), 'HORIZONTAL', 0.1, 0, 0, 250, 500),
    (0, 50, (1, 10, 10), 'VERTICAL', 0.1, 0, 0, 0, 0),
    (0, 51, (0, 11, 10), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 52, (0, 6, 11), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 53, (0, 7, 11), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 54, (0, 8, 11), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 55, (0, 9, 11), 'HORIZONTAL', 0.1, 0, 0, 500, 500),
    (0, 56, (0, 10, 11), 'HORIZONTAL', 0.1, 0, 0, 500, 500)]

stage, evap, runoff, withdrawal, rate = 110., 0.0103, 1000., 10000., -225000.0
lakeperioddata0 = [(0, 'status', 'active'),
                   (0, 'stage', stage),
                   (0, 'rainfall', recharge),
                   (0, 'evaporation', evap),
                   (0, 'runoff', runoff),
                   (0, 'withdrawal', withdrawal),
                   (0, 'rate', rate),
                   (0, 'AUXILIARY', 'temperature', temp),
                   (0, 'AUXILIARY', 'salinity', conc)]
lakeperioddata1 = [(0, 'rainfall', 0.0)]
lakeperioddata2 = [(0, 'rainfall', recharge)]
lakeperioddata3 = [(0, 'withdrawal', -rate), (0, 'rate', -withdrawal)]
lakeperioddata = {0: lakeperioddata0, 1: lakeperioddata1,
                  2: lakeperioddata2, 3: lakeperioddata3}

lakeperioddatats0 = [(0, 'status', 'active'),
                     (0, 'stage', 'stage'),
                     (0, 'rainfall', 'rainfall'),
                     (0, 'evaporation', 'evap'),
                     (0, 'runoff', 'runoff'),
                     (0, 'withdrawal', 'withdrawal'),
                     (0, 'rate', 'outlet'),
                     (0, 'AUXILIARY', 'salinity', 'concentration'),
                     (0, 'AUXILIARY', 'temperature', 'temperature')]
lakeperioddatats1 = [(0, 'rainfall', 0.0),
                     (0, 'rate', rate)]
lakeperioddatats2 = [(0, 'rainfall', 'rainfall2'),
                     (0, 'rate', 'outlet')]
lakeperioddatats3 = [(0, 'stage', 'stage'),
                     (0, 'rainfall', 'rainfall'),
                     (0, 'evaporation', 'evap'),
                     (0, 'runoff', 'runoff'),
                     (0, 'withdrawal', 'outlet2'),
                     (0, 'rate', 'withdrawal2')]
lakeperioddatats = {0: lakeperioddatats0, 1: lakeperioddatats1,
                    2: lakeperioddatats2, 3: lakeperioddatats3}

ts_names = ['stage', 'rainfall', 'evap', 'runoff', 'withdrawal', 'outlet',
            'concentration', 'temperature', 'rainfall2',
            'outlet2', 'withdrawal2']
ts_methods = ['linearend'] * len(ts_names)

ts_data = []
ts_times = np.arange(0., sim_time + 2. * pertime, pertime, dtype=float)
for t in ts_times:
    ts_data.append((t, stage, recharge, evap, runoff, withdrawal, rate,
                    temp, conc, recharge, -rate, -withdrawal))

lak_obs = {'lak_obs.csv': [('lake1', 'STAGE', (0,))]}


def build_model(ws, name, timeseries=False):
    hdsfile = '{}.hds'.format(name)

    # build the model
    sim = flopy.mf6.MFSimulation(sim_name=name, exe_name='mf6', sim_ws=ws)
    tdis = flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=period_data)
    ims = flopy.mf6.ModflowIms(sim, print_option='NONE',
                               linear_acceleration='CG',
                               outer_maximum=500, inner_maximum=100,
                               outer_dvclose=1e-6,
                               inner_dvclose=1e-3,
                               rcloserecord=[0.01, 'strict'])
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc, top=top, botm=botm,
                                  idomain=idomain)
    npf = flopy.mf6.ModflowGwfnpf(gwf, k=kh, icelltype=icelltype)
    sto = flopy.mf6.ModflowGwfsto(gwf, storagecoefficient=storage_coefficient,
                                  sy=sy, ss=ss, transient={0: True},
                                  iconvert=iconvert)
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)
    rch = flopy.mf6.ModflowGwfrcha(gwf, recharge=recharge)
    if timeseries:
        lakpd = lakeperioddatats
    else:
        lakpd = lakeperioddata
    lak = flopy.mf6.ModflowGwflak(gwf,
                                  nlakes=nlakes,
                                  noutlets=noutlets,
                                  print_input=True,
                                  print_stage=True,
                                  print_flows=True,
                                  auxiliary=['temperature', 'salinity'],
                                  packagedata=packagedata,
                                  connectiondata=connectiondata,
                                  outlets=outlets,
                                  perioddata=lakpd,
                                  pname='lak-1')
    lak.obs.initialize(filename='{}.lak.obs'.format(name), digits=20,
                       print_input=True, continuous=lak_obs)
    if timeseries:
        fname = '{}.lak.ts'.format(name)
        lak.ts.initialize(filename=fname, timeseries=ts_data,
                          time_series_namerecord=ts_names,
                          interpolation_methodrecord=ts_methods)

    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                head_filerecord='{}.hds'.format(name),
                                budget_filerecord='{}.cbc'.format(name),
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('BUDGET', 'LAST')])

    return sim


def get_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = build_model(ws, name)

    # build MODFLOW 6 files with UZF package
    ws = os.path.join(dir, 'mf6')
    mc = build_model(ws, name, timeseries=True)

    return sim, mc


def eval_budget(sim):
    print('evaluating budgets...')

    fname = '{}.cbc'.format(os.path.basename(sim.name))

    # open first cbc file
    fpth = os.path.join(sim.simpath, fname)
    cobj0 = flopy.utils.CellBudgetFile(fpth, precision='double')

    # open second cbc file
    fpth = os.path.join(sim.simpath, 'mf6', fname)
    cobj1 = flopy.utils.CellBudgetFile(fpth, precision='double')

    # build list of cbc data to retrieve
    avail = cobj1.get_unique_record_names()

    # initialize list for storing totals for each budget term terms
    cbc_keys = []
    for t in avail:
        if isinstance(t, bytes):
            t = t.decode()
        t = t.strip()
        cbc_keys.append(t)

    # set criteria
    dtol = 1e-6
    diffmax = 0.
    difftag = 'None'
    difftime = None
    fail = False

    # open a summary file and write header
    fname = '{}.cbc.cmp.out'.format(os.path.basename(sim.name))
    fpth = os.path.join(sim.simpath, fname)
    f = open(fpth, 'w')
    line = '{:15s}'.format('Time')
    line += '{:15s}'.format('Datatype')
    line += ' {:15s}'.format('Variables')
    line += ' {:15s}'.format('Timeseries')
    line += ' {:15s}'.format('Difference')
    f.write(line + '\n')
    f.write(len(line) * '-' + '\n')

    # get data from cbc file
    kk = cobj0.get_kstpkper()
    times = cobj0.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for key in cbc_keys:
            v0 = cobj0.get_data(kstpkper=k, text=key)[0]
            v1 = cobj1.get_data(kstpkper=k, text=key)[0]
            if isinstance(v0, np.recarray):
                v0 = v0['q'].sum()
                v1 = v1['q'].sum()
            else:
                v0 = v0.flatten().sum()
                v1 = v1.flatten().sum()

            diff = v0 - v1
            if abs(diff) > abs(diffmax):
                diffmax = diff
                difftag = key
                difftime = t
            if abs(diff) > dtol:
                fail = True
            line = '{:15g}'.format(t)
            line += '{:15s}'.format(key)
            line += ' {:15g}'.format(v0)
            line += ' {:15g}'.format(v1)
            line += ' {:15g}'.format(diff)
            f.write(line + '\n')

    msg = 'Maximum cbc difference:        {}\n'.format(diffmax)
    msg += 'Maximum cbc difference time:   {}\n'.format(difftime)
    msg += 'Maximum cbc datatype:          {}\n'.format(difftag)
    if fail:
        msg += 'Maximum cbc citeria exceeded:  {}\n'.format(dtol)
    assert not fail, msg

    # close summary file and print the final message
    f.close()
    print(msg)

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
        yield test.run_mf6, Simulation(dir, exfunc=eval_budget, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_budget, idxsim=idx)
        test.run_mf6(sim)
    return


# use python testmf6_drn_ddrn01.py
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
