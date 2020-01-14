# Simple one-layer model with a lak.  Purpose is to test a constant
# stage and constant concentration lake with a value of 100.  The aquifer
# starts with a concentration of zero, but the values grow as the lake
# leaks into the aquifer.

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

ex = ['lkt_01']
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
    delz = lz / nlay
    top = [0., 0., -0.90, 0., 0.]
    botm = list(top - np.arange(delz, nlay * delz + delz, delz))
    botm[2] = -1.0

    perlen = [0.1]
    nstp = [10]
    kstp = perlen[0] / nstp[0]
    tsmult = [1.]

    lakevap = 0.  # 0.008
    lakewid0 = 1
    rchrt = 0.  # 0.0005
    Kh = 20.
    Kv = 20.

    steady = [True]
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    single_matrix = False
    nouter, ninner = 700, 300
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

    gwf = flopy.mf6.MFModel(sim, model_type='gwf6', modelname=gwfname,
                            model_nam_file='{}.nam'.format(gwfname))

    imsgwf = flopy.mf6.ModflowIms(sim, print_option='ALL',
                                  outer_hclose=hclose,
                                  outer_maximum=nouter,
                                  under_relaxation='NONE',
                                  inner_maximum=ninner,
                                  inner_hclose=hclose, rcloserecord=rclose,
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
    ic = flopy.mf6.ModflowGwfic(gwf, strt=0.)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, xt3doptions=False,
                                  save_flows=True,
                                  save_specific_discharge=True,
                                  icelltype=0,
                                  k=Kh, k33=Kv)

    # chd files
    chdlist1 = [[(0, 0, 0), -0.5],
                [(0, 0, ncol - 1), -0.5],
                ]
    chd1 = flopy.mf6.ModflowGwfchd(gwf,
                                   stress_period_data=chdlist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='CHD-1',
                                   auxiliary='CONCENTRATION',
                                   filename='{}.chd'.format(gwfname))

    nlakeconn = 3  # note: this is the number of cells, not total number of connections
    # pak_data = [lakeno, strt, nlakeconn, CONC, dense, boundname]
    pak_data = [(0, -0.4, nlakeconn, 0., 1025.)]

    connlen = connwidth = delr / 2.
    con_data = []
    # con_data=(lakeno,iconn,(cellid),claktype,bedleak,belev,telev,connlen,connwidth )
    con_data.append(
        (0, 0, (0, 0, 1), 'HORIZONTAL', 'None', 10, 10, connlen, connwidth))
    con_data.append(
        (0, 1, (0, 0, 3), 'HORIZONTAL', 'None', 10, 10, connlen, connwidth))
    con_data.append(
        (0, 2, (0, 0, 2), 'VERTICAL', 'None', 10, 10, connlen, connwidth))
    p_data = [(0, 'STATUS', 'CONSTANT'),
              (0, 'STAGE', -0.4),
              (0, 'RAINFALL', .1),
              (0, 'EVAPORATION', .2),
              (0, 'RUNOFF', .1 * delr * delc),
              (0, 'WITHDRAWAL', .1),
              ]
    # <outletno> <lakein> <lakeout> <couttype> <invert> <width> <rough> <slope>
    outlets = [(0, 0, -1, 'SPECIFIED', 999., 999., 999., 999.)]
    outletperioddata = [(0, 'RATE', -0.1)]

    # note: for specifying lake number, use fortran indexing!
    lak_obs = {('lak_obs.csv'): [('lakestage', 'stage', 1),
                                 ('lakevolume', 'volume', 1),
                                 ('lak1', 'lak', 1, 1),
                                 ('lak2', 'lak', 1, 2),
                                 ('lak3', 'lak', 1, 3)]}

    lak = flopy.mf6.modflow.ModflowGwflak(gwf, save_flows=True,
                                          print_input=True,
                                          print_flows=True,
                                          print_stage=True,
                                          stage_filerecord='stage',
                                          budget_filerecord='lakebud',
                                          nlakes=1, ntables=0, noutlets=1,
                                          packagedata=pak_data,
                                          outlets=outlets,
                                          pname='LAK-1',
                                          connectiondata=con_data,
                                          lakeperioddata=p_data,
                                          outletperioddata=outletperioddata,
                                          observations=lak_obs,
                                          auxiliary=['CONCENTRATION',
                                                     'DENSITY'])

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(gwfname),
                                head_filerecord='{}.hds'.format(gwfname),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL', 'STEPS'),
                                            ('BUDGET', 'ALL', 'STEPS')],
                                printrecord=[('HEAD', 'LAST', 'STEPS'),
                                             ('BUDGET', 'LAST', 'STEPS')])

    # create gwt model
    gwtname = 'gwt_' + name
    gwt = flopy.mf6.MFModel(sim, model_type='gwt6', modelname=gwtname,
                            model_nam_file='{}.nam'.format(gwtname))

    if not single_matrix:
        imsgwt = flopy.mf6.ModflowIms(sim, print_option='ALL',
                                      outer_hclose=hclose,
                                      outer_maximum=nouter,
                                      under_relaxation='NONE',
                                      inner_maximum=ninner,
                                      inner_hclose=hclose, rcloserecord=rclose,
                                      linear_acceleration='BICGSTAB',
                                      scaling_method='NONE',
                                      reordering_method='NONE',
                                      relaxation_factor=relax,
                                      filename='{}.ims'.format(gwtname))
        sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(gwt, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm, idomain=idomain)

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.,
                                filename='{}.ic'.format(gwtname))

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme='UPSTREAM',
                                  filename='{}.adv'.format(gwtname))

    # dispersion
    # diffc = 0.0
    # dsp = flopy.mf6.ModflowGwtdsp(gwt, xt3d=True, diffc=diffc,
    #                              alh=0.1, ath1=0.01, atv=0.05,
    #                              filename='{}.dsp'.format(gwtname))

    # storage
    porosity = 0.30
    sto = flopy.mf6.ModflowGwtmst(gwt, porosity=porosity,
                                  filename='{}.sto'.format(gwtname))
    # sources
    sourcerecarray = [('CHD-1', 'AUX', 'CONCENTRATION'),
                      #('WEL-1', 'AUX', 'CONCENTRATION'),
                      ]
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray,
                                  filename='{}.ssm'.format(gwtname))

    lktpackagedata = [(0, 35., 99., 999., 'mylake'), ]
    lktperioddata = [(0, 'STATUS', 'CONSTANT'),
                     (0, 'CONCENTRATION', 100.),
                     (0, 'RAINFALL', 25.),
                     (0, 'EVAPORATION', 25.),
                     (0, 'RUNOFF', 25.),
                     ]

    lkt_obs = {(gwtname + '.lkt.obs.csv', ):
                   [
                    ('lkt-1-conc', 'CONCENTRATION', 1),
                    ('lkt-1-extinflow', 'EXT-INFLOW', 1),
                    ('lkt-1-rain', 'RAINFALL', 1),
                    ('lkt-1-roff', 'RUNOFF', 1),
                    ('lkt-1-evap', 'EVAPORATION', 1),
                    ('lkt-1-wdrl', 'WITHDRAWAL', 1),
                    ('lkt-1-stor', 'STORAGE', 1),
                    ('lkt-1-const', 'CONSTANT', 1),
                    ('lkt-1-gwt2', 'LKT', 1, 1),
                    ('lkt-1-gwt4', 'LKT', 1, 3),
                    ('lkt-1-gwt3', 'LKT', 1, 2),
                    ('lkt-1-mylake', 'LKT', 'MYLAKE'),
                   ],
               #'ghb_flows.csv': [('Estuary2', 'GHB', 'Estuary-L2'),
               #                  ('Estuary3', 'GHB', 'Estuary-L3')]
               }
    # append additional obs attributes to obs dictionary
    lkt_obs['digits'] = 7
    lkt_obs['print_input'] = True
    lkt_obs['filename'] = gwtname + '.lkt.obs'

    lkt = flopy.mf6.modflow.ModflowGwtlkt(gwt,
                                          boundnames=True,
                                          save_flows=True,
                                          print_input=True,
                                          print_flows=True,
                                          print_concentration=True,
                                          concentration_filerecord=gwtname + '.lkt.bin',
                                          budget_filerecord='gwtlak1.bud',
                                          packagedata=lktpackagedata,
                                          lakeperioddata=lktperioddata,
                                          observations=lkt_obs,
                                          pname='LAK-1',
                                          auxiliary=['aux1', 'aux2'])
    # output control
    oc = flopy.mf6.ModflowGwtoc(gwt,
                                budget_filerecord='{}.cbc'.format(gwtname),
                                concentration_filerecord='{}.ucn'.format(
                                    gwtname),
                                concentrationprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('CONCENTRATION', 'ALL', 'STEP')],
                                printrecord=[('CONCENTRATION', 'ALL', 'STEP'),
                                             ('BUDGET', 'ALL', 'STEP')])

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(sim, exgtype='GWF6-GWT6',
                                     exgmnamea=gwfname, exgmnameb=gwtname,
                                     filename='{}.gwfgwt'.format(name))



    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def eval_results(sim):
    print('evaluating results...')

    # ensure lake concentrations were saved
    name = ex[sim.idxsim]
    gwtname = 'gwt_' + name
    fname = gwtname + '.lkt.bin'
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)

    # load the lake concentrations and make sure all values are 100.
    cobj = flopy.utils.HeadFile(fname, text='CONCENTRATION')
    clak = cobj.get_alldata().flatten()
    answer = np.ones(10)*100.
    assert np.allclose(clak, answer), '{} {}'.format(clak, answer)

    # load the aquifer concentrations and make sure all values are correct
    fname = gwtname + '.ucn'
    fname = os.path.join(sim.simpath, fname)
    cobj = flopy.utils.HeadFile(fname, text='CONCENTRATION')
    caq = cobj.get_alldata()
    answer = np.array([ 4.86242795, 27.24270616, 64.55536421,
                        27.24270616, 4.86242795])
    assert np.allclose(caq[-1].flatten(), answer), '{} {}'.format(caq[-1].flatten(), answer)

    # lkt observation results
    fpth = os.path.join(sim.simpath, gwtname + '.lkt.obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)
    res = tc['LKT1CONC']
    answer = np.ones(10) * 100.
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1EXTINFLOW']
    answer = np.ones(10) * 0.
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1RAIN']
    answer = np.ones(10) * 2.5
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1ROFF']
    answer = np.ones(10) * 2.5
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1EVAP']
    answer = np.ones(10) * -5.
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1WDRL']
    answer = np.ones(10) * -10.
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1STOR']
    answer = np.ones(10) * 0.
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1CONST']
    answer = np.ones(10) * 236.3934
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1GWT2']
    answer = np.ones(10) * -91.80328
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1GWT4']
    answer = np.ones(10) * -32.78689
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1GWT3']
    answer = np.ones(10) * -91.80328
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1MYLAKE']
    answer = np.ones(10) * -216.3934
    assert np.allclose(res, answer), '{} {}'.format(res, answer)

    # todo: add a better check of the lake concentrations
    # assert False

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
