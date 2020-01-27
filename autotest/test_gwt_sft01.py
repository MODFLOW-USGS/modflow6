# Simple one-layer model with sfr.  Purpose is to test transport in a one-d
# sfr network.
# Just in the beginning stages of getting this to work -- need to turn on
# sft package and start testing for transport down stream channel

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

ex = ['sft_01']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))


def get_model(idx, dir):
    lx = 7.
    lz = 1.
    nlay = 1
    nrow = 1
    ncol = 7
    nper = 1
    delc = 1.
    delr = lx / ncol
    delz = lz / nlay
    top = [0., 0., 0., 0., 0., 0., 0.]
    botm = list(top - np.arange(delz, nlay * delz + delz, delz))
    botm[2] = -1.0

    perlen = [0.1]
    nstp = [10]
    kstp = perlen[0] / nstp[0]
    tsmult = [1.]

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
    chdlist1 = [[(0, 0, 0), 0., 0.],
                [(0, 0, ncol - 1), -0.5, 0.],
                ]
    chd1 = flopy.mf6.ModflowGwfchd(gwf,
                                   stress_period_data=chdlist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='CHD-1',
                                   auxiliary='CONCENTRATION',
                                   filename='{}.chd'.format(gwfname))

    # pak_data = [<rno> <cellid(ncelldim)> <rlen> <rwid> <rgrd> <rtp> <rbth> <rhk> <man> <ncon> <ustrf> <ndv> [<aux(naux)>] [<boundname>]]
    rlen = delr
    rwid = delc
    rgrd = 0.01
    rtp = 0.
    rbth = 0.1
    rhk = 0. #0.01
    rman = 0.2
    ncon = 2
    ustrf = 1.
    ndv = 0
    pak_data = []
    for irno in range(ncol):
        ncon = 2
        if irno in [0, ncol - 1]:
            ncon = 1
        cellid = (0, 0, irno)
        t = (irno, cellid, rlen, rwid, rgrd, rtp, rbth, rhk, rman, ncon, ustrf, ndv)
        pak_data.append(t)


    con_data = []
    for irno in range(ncol):
        if irno == 0:
            t = (irno, -(irno + 1))
        elif irno == ncol - 1:
            t = (irno, irno - 1)
        else:
            t = (irno, irno - 1, -(irno + 1))
        con_data.append(t)


    p_data = [
              (0, 'INFLOW', 1.),
              ]

    # note: for specifying sfr number, use fortran indexing!
    #sfr_obs = {('sfr_obs.csv'): [('lakestage', 'stage', 1),
    #                             ('lakevolume', 'volume', 1),
    #                             ('lak1', 'lak', 1, 1),
    #                             ('lak2', 'lak', 1, 2),
    #                             ('lak3', 'lak', 1, 3)]}

    sfr = flopy.mf6.modflow.ModflowGwfsfr(gwf, save_flows=True,
                                          print_input=True,
                                          print_flows=True,
                                          print_stage=True,
                                          stage_filerecord=gwfname+'.sfr.stg',
                                          budget_filerecord=gwfname+'.sfr.bud',
                                          nreaches=ncol,
                                          packagedata=pak_data,
                                          pname='SFR-1',
                                          connectiondata=con_data,
                                          perioddata=p_data,
                                          #observations=lak_obs,
                                          #auxiliary=['CONCENTRATION',
                                          #           'DENSITY'],
                                          )

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
    ic = flopy.mf6.ModflowGwtic(gwt, strt=[0., 0., 0., 0., 0., 0., 0.],
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

    sftpackagedata = []
    for irno in range(ncol):
        t = (irno, 0., 99., 999., 'myreach{}'.format(irno + 1))
        sftpackagedata.append(t)

    sftperioddata = [
                     (0, 'STATUS', 'CONSTANT'),
                     (0, 'CONCENTRATION', 100.)
                     ]

    sft_obs = {(gwtname + '.sft.obs.csv', ):
                   [
                    ('sft-1-conc', 'CONCENTRATION', 1),
                    ('sft-1-extinflow', 'EXT-INFLOW', 1),
                    ('sft-1-rain', 'RAINFALL', 1),
                    ('sft-1-roff', 'RUNOFF', 1),
                    ('sft-1-evap', 'EVAPORATION', 1),
                    ('sft-1-const', 'CONSTANT', 1),
                    ('sft-1-gwt2', 'SFT', 1, 1),
                    ('sft-1-gwt4', 'SFT', 1, 3),
                    ('sft-1-gwt3', 'SFT', 1, 2),
                    ('sft-1-mylake1', 'SFT', 'MYREACH1'),
                   ],
               }
    # append additional obs attributes to obs dictionary
    sft_obs['digits'] = 7
    sft_obs['print_input'] = True
    sft_obs['filename'] = gwtname + '.sft.obs'

    sft = flopy.mf6.modflow.ModflowGwtsft(gwt,
                                          boundnames=True,
                                          save_flows=True,
                                          print_input=True,
                                          print_flows=True,
                                          print_concentration=True,
                                          concentration_filerecord=gwtname + '.sft.bin',
                                          budget_filerecord=gwtname + '.sft.bud',
                                          packagedata=sftpackagedata,
                                          reachperioddata=sftperioddata,
                                          observations=sft_obs,
                                          pname='SFR-1',
                                          auxiliary=['aux1', 'aux2'])

    # output control
    oc = flopy.mf6.ModflowGwtoc(gwt,
                                budget_filerecord='{}.cbc'.format(gwtname),
                                concentration_filerecord='{}.ucn'.format(
                                    gwtname),
                                concentrationprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('CONCENTRATION', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('CONCENTRATION', 'ALL'),
                                             ('BUDGET', 'ALL')])

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

    # load the lake concentrations and make sure all values are correct
    cobj = flopy.utils.HeadFile(fname, text='CONCENTRATION')
    clak = cobj.get_data()
    answer = np.array([2.20913605e-01, 2.06598617e-03, 1.64112298e-05])
    assert np.allclose(clak, answer), '{} {}'.format(clak, answer)

    # load the aquifer concentrations and make sure all values are correct
    fname = gwtname + '.ucn'
    fname = os.path.join(sim.simpath, fname)
    cobj = flopy.utils.HeadFile(fname, text='CONCENTRATION')
    caq = cobj.get_data()
    answer = np.array([1.00000000e+02, 8.50686091e+00, 5.71594204e-01,
                       1.30062708e-02, 2.38399700e-04, 3.30711200e-06,
                       7.33445279e-08])
    assert np.allclose(caq, answer), '{} {}'.format(caq.flatten(), answer)

    # lkt observation results
    fpth = os.path.join(sim.simpath, gwtname + '.lkt.obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)
    res = tc['LKT1CONC']
    answer = [0.00418347, 0.01249363, 0.02487425, 0.04126975, 0.06162508,
              0.0858858,  0.113998,   0.1459085,  0.1815644,  0.2209136 ]
    answer = np.array(answer)
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1STOR']
    answer = [-0.1988482, -0.3949968, -0.588474,  -0.779308,  -0.9675264,
              -1.153157, -1.336226,  -1.516762,  -1.694791,  -1.87034]
    answer = np.array(answer)
    assert np.allclose(res, answer), '{} {}'.format(res, answer)
    res = tc['LKT1MYLAKE1']
    answer = [0.1992666, 0.3962462, 0.5909615, 0.7834349, 0.9736889, 1.161745,
              1.347626, 1.531353,  1.712948,  1.892431]
    answer = np.array(answer)
    assert np.allclose(res, answer), '{} {}'.format(res, answer)

    # load the lake budget file
    fname = gwtname + '.lkt.bud'
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.CellBudgetFile(fname, precision='double', verbose=False)
    # check the flow-ja-face terms
    res = bobj.get_data(text='flow-ja-face')[-1]
    answer = [(1, 2, -0.02209136), (2, 1,  0.02209136), (2, 3, -0.0002066 ),
              (3, 2,  0.0002066)]
    dt = [('node', '<i4'), ('node2', '<i4'), ('q', '<f8')]
    answer = np.array(answer, dtype=dt)
    for dtname, dttype in dt:
        assert np.allclose(res[dtname], answer[dtname]), '{} {}'.format(res, answer)
    # check the storage terms, which include the total mass in the lake as an aux variable
    res = bobj.get_data(text='storage')[-1]
    answer = [(1, 1, -1.87033970e+00, 1.05004295e-01),
              (2, 2, -2.18847617e-02, 8.85953709e-04),
              (3, 3, -2.10987695e-04, 6.88867607e-06)]
    dt = [('node', '<i4'), ('node2', '<i4'), ('q', '<f8'), ('MASS', '<f8')]
    answer = np.array(answer, dtype=dt)
    for dtname, dttype in dt:
        assert np.allclose(res[dtname], answer[dtname]), '{} {}'.format(res, answer)


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
