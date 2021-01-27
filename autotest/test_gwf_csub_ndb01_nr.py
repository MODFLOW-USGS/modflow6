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

from framework import testing_framework, running_on_CI
from simulation import Simulation

ex = (
    "csub_ndb01a",
    "csub_ndb01b",
    "csub_ndb01c",
    "csub_ndb01d",
    "csub_ndb01e",
    "csub_ndb01f",
    "csub_ndb01g",
    "csub_ndb01h",
)
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
newtons = (
    True,
    False,
    True,
    False,
    True,
    False,
    True,
    False,
)
stress_lag = (
    None,
    None,
    True,
    True,
    None,
    None,
    True,
    True,
)
elastic = (
    True,
    True,
    True,
    True,
    False,
    False,
    False,
    False,
)

ddir = 'data'

# set replace_exe to None to use default executable
replace_exe = None

htol = None
dtol = 1e-3
budtol = 0.01

bud_lst = (
    "CSUB-CGELASTIC_IN",
    "CSUB-CGELASTIC_OUT",
)

# static model data
nlay, nrow, ncol = 2, 1, 2
nper = 3
tsp0 = 1.
perlen = [tsp0] + [365.2500000 for i in range(nper - 1)]
nstp = [1] + [200 for i in range(nper - 1)]
tsmult = [1.0] + [1.0 for i in range(nper - 1)]
steady = [True] + [False for i in range(nper - 1)]
delr, delc = 1000., 1000.
top = 0.
botm = [-10., -20.]
zthick = [top - botm[0],
          botm[0] - botm[1]]
strt = -5.
hmin = -15.

shape3d = (nlay, nrow, ncol)

# npf variables
hk = 1e4
laytyp = 1
sy = 0.

nouter, ninner = 50, 300
hclose, rclose, relax = 1e-9, 1e-3, 1.

tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# all cells are active
ib = 1

# chd data
ts_name = "CHD_TS"
c6 = [
    [1, 0, 0, ts_name]
]
cd6 = {0: c6}
maxchd = len(cd6[0])

# static sub data
sgm = 1.7
sgs = 2.0
void = 0.82
theta = void / (1. + void)
beta = 0.
crnd0 = 0.
cc, cr = 6e-4, 6e-6
ini_stress = 0.
kv = 999.
H0 = 0.


def get_model(idx, dir):
    name = ex[idx]
    newton = newtons[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version='mf6',
        exe_name='mf6',
        sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS",
        nper=nper, perioddata=tdis_rc
    )

    # create iterative model solution and register the gwf model with it
    if newton:
        newtonoptions = ""
        imsla = "BICGSTAB"
        rewet_record = None
        wetdry = None
    else:
        newtonoptions = None
        imsla = "CG"
        rewet_record = [
            "wetfct",
            0.1,
            "iwetit",
            1,
            "ihdwet",
            0,
        ]
        wetdry = [1, 0]

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=[rclose, "strict"],
        linear_acceleration=imsla,
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        newtonoptions=newtonoptions
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf, nlay=nlay, nrow=nrow, ncol=ncol,
        delr=delr, delc=delc,
        top=top, botm=botm,
    )

    obs_recarray = {
        'gwf_obs.csv':
            [
                ('h2_1_1', 'HEAD', (1, 0, 0)),
                ('h1_1_2', 'HEAD', (0, 0, 1)),
                ('h2_1_2', 'HEAD', (1, 0, 1)),
            ]
    }
    obs_package = flopy.mf6.ModflowUtlobs(gwf, pname='head_obs',
                                          digits=10, print_input=True,
                                          continuous=obs_recarray)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=False,
        icelltype=laytyp,
        k=hk,
        rewet_record=rewet_record,
        wetdry=wetdry,
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=0.,
        sy=sy,
        steady_state={0: True},
        transient={1: True}
    )

    # create chd time series
    chnam = '{}.ch.ts'.format(name)
    chd_ts = [
        (0., strt),
        (tsp0, strt),
        (365.25 + tsp0, hmin),
        (np.array(perlen).sum(), strt)]

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
        gwf,
        maxbound=maxchd,
        stress_period_data=cd6,
        save_flows=False
    )

    # initialize time series
    chd.ts.initialize(filename=chnam, timeseries=chd_ts,
                      time_series_namerecord=[ts_name, ],
                      interpolation_methodrecord=['linear'])

    # csub observations
    obspos = [(0, 0, 1), (1, 0, 1), ]
    obstype = ['compaction-cell', 'csub-cell']
    obstag = ['tcomp', 'csub']
    obsarr = []
    for iobs, cobs in enumerate(obstype):
        for jobs, otup in enumerate(obspos):
            otag = '{}{}'.format(obstag[iobs], jobs + 1)
            obsarr.append((otag, cobs, otup))

    # csub files
    swt6 = []
    ibcno = 0
    if elastic[idx]:
        cct, crt = cr, cr
    else:
        cct, crt = cc, cr
    for k in range(nlay):
        d = [
            ibcno,
            (k, 0, 1),
            "nodelay",
            ini_stress,
            zthick[k],
            1.,
            cct,
            crt,
            theta,
            kv,
            H0
        ]
        swt6.append(d)
        ibcno += 1

    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        observations={"csub_obs.csv": obsarr},
        effective_stress_lag=stress_lag[idx],
        save_flows=True,
        beta=beta,
        sgm=sgm,
        sgs=sgs,
        ninterbeds=len(swt6),
        cg_theta=theta,
        cg_ske_cr=crnd0,
        packagedata=swt6,
    )

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


def eval_comp(sim):
    print('evaluating compaction...')

    # MODFLOW 6 total compaction results
    fpth = os.path.join(sim.simpath, 'csub_obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # get results from listing file
    fpth = os.path.join(sim.simpath,
                        '{}.lst'.format(os.path.basename(sim.name)))
    budl = flopy.utils.Mf6ListBudget(fpth)
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]

    # get results from cbc file
    cbc_bud = ('CSUB-CGELASTIC',)
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.
    fpth = os.path.join(sim.simpath,
                        '{}.cbc'.format(os.path.basename(sim.name)))
    cobj = flopy.utils.CellBudgetFile(fpth, precision='double')
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.
            qout = 0.
            v = cobj.get_data(kstpkper=k, text=text)[0]
            for kk in range(v.shape[0]):
                for ii in range(v.shape[1]):
                    for jj in range(v.shape[2]):
                        vv = v[kk, ii, jj]
                        if vv < 0.:
                            qout -= vv
                        else:
                            qin += vv
            d['totim'][idx] = t
            d['time_step'][idx] = k[0]
            d['stress_period'] = k[1]
            key = '{}_IN'.format(text)
            d[key][idx] = qin
            key = '{}_OUT'.format(text)
            d[key][idx] = qout

    diff = np.zeros((nbud, len(bud_lst)), dtype=np.float)
    for idx, key in enumerate(bud_lst):
        diff[:, idx] = d0[key] - d[key]
    diffmax = np.abs(diff).max()
    msg = 'maximum absolute total-budget difference ({}) '.format(diffmax)

    # write summary
    fpth = os.path.join(sim.simpath,
                        '{}.bud.cmp.out'.format(os.path.basename(sim.name)))
    f = open(fpth, 'w')
    for i in range(diff.shape[0]):
        if i == 0:
            line = '{:>10s}'.format('TIME')
            for idx, key in enumerate(bud_lst):
                line += '{:>25s}'.format(key + '_LST')
                line += '{:>25s}'.format(key + '_CBC')
                line += '{:>25s}'.format(key + '_DIF')
            f.write(line + '\n')
        line = '{:10g}'.format(d['totim'][i])
        for idx, key in enumerate(bud_lst):
            line += '{:25g}'.format(d0[key][i])
            line += '{:25g}'.format(d[key][i])
            line += '{:25g}'.format(diff[i, idx])
        f.write(line + '\n')
    f.close()

    if diffmax > budtol:
        sim.success = False
        msg += 'exceeds {}'.format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print('    ' + msg)

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def test_mf6model():
    # determine if running on Travis or GitHub actions
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_comp,
                                       exe_dict=r_exe,
                                       htol=htol,
                                       idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_comp,
                         exe_dict=replace_exe, htol=htol, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
