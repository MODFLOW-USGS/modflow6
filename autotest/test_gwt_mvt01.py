# Simple one-layer model with a lak and sfr network on top.  Purpose is to
# test movement of solute between advanced packages.  In this case water
# from a lake outlet is moved into the first sfr reach.  The test confirms
# that the solute from the lake is moved into the sfr reach.
# There is no flow between the stream and the aquifer.

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

ex = ['mvt_01']
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
                                  outer_dvclose=hclose,
                                  outer_maximum=nouter,
                                  under_relaxation='NONE',
                                  inner_maximum=ninner,
                                  inner_dvclose=hclose, rcloserecord=rclose,
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
    chdlist1 = [
                [(0, 0, ncol - 1), 0., 0.],
                ]
    chd1 = flopy.mf6.ModflowGwfchd(gwf,
                                   stress_period_data=chdlist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='CHD-1',
                                   auxiliary='CONCENTRATION',
                                   filename='{}.chd'.format(gwfname))

    # wel files
    wellist1 = [
                [(0, 0, 0), 1., 100.],
                ]
    wel1 = flopy.mf6.ModflowGwfwel(gwf,
                                   stress_period_data=wellist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='WEL-1',
                                   auxiliary='CONCENTRATION',
                                   filename='{}.wel'.format(gwfname))


    nlakeconn = 1  # note: this is the number of connectiosn for a lake, not total number of connections
    # pak_data = [lakeno, strt, nlakeconn, CONC, dense, boundname]
    pak_data = [(0, 1.0, nlakeconn, 0., 1025.),
                (1, 1.0, nlakeconn, 0., 1025.),]

    connlen = connwidth = delr / 2.
    con_data = []
    # con_data=(lakeno,iconn,(cellid),claktype,bedleak,belev,telev,connlen,connwidth )
    con_data.append((0, 0, (0, 0, 0), 'VERTICAL', 0., 0, 0, connlen, connwidth))
    con_data.append((1, 0, (0, 0, ncol - 1), 'VERTICAL', 0., 0, 0, connlen, connwidth))
    p_data = [
              (0, 'STATUS', 'CONSTANT'),
              (0, 'STAGE', 1.0),
              (1, 'STATUS', 'CONSTANT'),
              (1, 'STAGE', 1.0),
              ]
    # <outletno> <lakein> <lakeout> <couttype> <invert> <width> <rough> <slope>
    outlets = [(0, 0, -1, 'SPECIFIED', 999., 999., 999., 999.)]
    outletperioddata = [(0, 'RATE', -1.0)]

    # note: for specifying lake number, use fortran indexing!
    lak_obs = {('lak_obs.csv'): [('lakestage', 'stage', 1),
                                 ('lakevolume', 'volume', 1),]
              }

    lak = flopy.mf6.modflow.ModflowGwflak(gwf, save_flows=True,
                                          print_input=True,
                                          print_flows=True,
                                          print_stage=True,
                                          mover=True,
                                          stage_filerecord='stage',
                                          budget_filerecord='lakebud',
                                          nlakes=2, ntables=0, noutlets=1,
                                          packagedata=pak_data,
                                          outlets=outlets,
                                          pname='LAK-1',
                                          connectiondata=con_data,
                                          perioddata=p_data + outletperioddata,
                                          observations=lak_obs,
                                          auxiliary=['CONCENTRATION',
                                                     'DENSITY'])

    # pak_data = [<rno> <cellid(ncelldim)> <rlen> <rwid> <rgrd> <rtp> <rbth> <rhk> <man> <ncon> <ustrf> <ndv> [<aux(naux)>] [<boundname>]]
    rlen = delr
    rwid = delc
    rgrd = 1.
    rtp = 0.
    rbth = 0.1
    rhk = 0. #0.01
    rman = 1.
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
              (0, 'INFLOW', 0.),
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
                                          mover=True,
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
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'LAST')])


    packages = [('lak-1',), ('sfr-1',), ]
    perioddata = [
        ('lak-1', 0, 'sfr-1', 0, 'factor', 1.),
        ('sfr-1', 6, 'lak-1', 1, 'factor', 1.),
                 ]
    mvr = flopy.mf6.ModflowGwfmvr(gwf, maxmvr=len(perioddata),
                                  budget_filerecord='{}.mvr.bud'.format(name),
                                  maxpackages=len(packages),
                                  print_flows=True,
                                  packages=packages,
                                  perioddata=perioddata)



    # create gwt model
    gwtname = 'gwt_' + name
    gwt = flopy.mf6.MFModel(sim, model_type='gwt6', modelname=gwtname,
                            model_nam_file='{}.nam'.format(gwtname))

    if not single_matrix:
        imsgwt = flopy.mf6.ModflowIms(sim, print_option='ALL',
                                      outer_dvclose=hclose,
                                      outer_maximum=nouter,
                                      under_relaxation='NONE',
                                      inner_maximum=ninner,
                                      inner_dvclose=hclose, rcloserecord=rclose,
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
    porosity = 1.0
    sto = flopy.mf6.ModflowGwtmst(gwt, porosity=porosity,
                                  filename='{}.sto'.format(gwtname))
    # sources
    sourcerecarray = [('CHD-1', 'AUX', 'CONCENTRATION'),
                      ('WEL-1', 'AUX', 'CONCENTRATION'),
                      ]
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray,
                                  filename='{}.ssm'.format(gwtname))


    # lkt package
    lktpackagedata = [(0, 0., 99., 999., 'mylake'),
                      (1, 0., 99., 999., 'mylake2'), ]
    lktperioddata = [(0, 'STATUS', 'CONSTANT'),
                     (0, 'CONCENTRATION', 100.),
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
                    ('lkt-1-mylake', 'LKT', 'MYLAKE'),
                   ],
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


    # sft
    sftpackagedata = []
    for irno in range(ncol):
        t = (irno, 0., 99., 999., 'myreach{}'.format(irno + 1))
        sftpackagedata.append(t)

    sftperioddata = [
                     (0, 'STATUS', 'ACTIVE'),
                     ]

    sft_obs = {(gwtname + '.sft.obs.csv', ):
                   [('sft-{}-conc'.format(i + 1), 'CONCENTRATION', i + 1) for i in range(7)] +
                   [
                    ('sft-1-extinflow', 'EXT-INFLOW', 1),
                    ('sft-1-rain', 'RAINFALL', 1),
                    ('sft-1-roff', 'RUNOFF', 1),
                    ('sft-1-evap', 'EVAPORATION', 1),
                    ('sft-1-const', 'CONSTANT', 1),
                    ('sft-1-gwt1', 'SFT', 1, 1),
                    ('sft-1-gwt2', 'SFT', 2, 1),
                    ('sft-1-gwt3', 'SFT', 3, 1),
                    ('sft-1-myreach1', 'SFT', 'MYREACH1'),
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

    # mover transport package
    mvt = flopy.mf6.modflow.ModflowGwtmvt(gwt, print_flows=True)

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
    fname = gwtname + '.sft.bin'
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)

    # load the lake concentrations and make sure all values are correct
    cobj = flopy.utils.HeadFile(fname, text='CONCENTRATION')
    csft = cobj.get_data().flatten()

    # load the aquifer concentrations and make sure all values are correct
    fname = gwtname + '.ucn'
    fname = os.path.join(sim.simpath, fname)
    cobj = flopy.utils.HeadFile(fname, text='CONCENTRATION')
    caq = cobj.get_data().flatten()

    assert np.allclose(csft, caq), '{} {}'.format(csft, caq)

    # sft observation results
    fpth = os.path.join(sim.simpath, gwtname + '.sft.obs.csv')
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=',')
    except:
        assert False, 'could not load data from "{}"'.format(fpth)
    # compare observation concs with binary file concs
    for i in range(7):
        oname = 'SFT{}CONC'.format(i + 1)
        assert np.allclose(tc[oname][-1], csft[i]), '{} {}'.format(tc[oname][-1], csft[i])

    # load the sft budget file
    fname = gwtname + '.sft.bud'
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.CellBudgetFile(fname, precision='double', verbose=False)
    # check the flow-ja-face terms
    res = bobj.get_data(text='flow-ja-face')[-1]
    #print(res)

    # check the storage terms, which include the total mass in the reach as an aux variable
    res = bobj.get_data(text='storage')[-1]
    #print(res)

    # get mvt results from listing file
    bud_lst = ['SFR-1_IN', 'SFR-1_OUT',
               'LAK-1_IN', 'LAK-1_OUT']
    fname = gwtname + '.lst'
    fname = os.path.join(sim.simpath, fname)
    budl = flopy.utils.Mf6ListBudget(fname, budgetkey='TRANSPORT MOVER BUDGET FOR ENTIRE MODEL')
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    errmsg = 'SFR-1_OUT NOT EQUAL LAK-1_IN\n{}\n{}'.format(d0['SFR-1_OUT'], d0['LAK-1_IN'])
    assert np.allclose(d0['SFR-1_OUT'], d0['LAK-1_IN'])

    # uncomment when testing so files aren't deleted
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
