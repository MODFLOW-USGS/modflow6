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

import targets

exe_name_mf = os.path.abspath('./temp/mfexes/mf2005')
exe_name_mt = os.path.abspath('./temp/mfexes/mt3dms')
exe_name_mf6 = targets.target_dict['mf6']
testdir = './temp'
testgroup = 'mt3dms_p01'


def p01mt3d(model_ws, al, retardation, lambda1, mixelm,
            zeta=None, prsity2=None):
    nlay = 1
    nrow = 1
    ncol = 101
    delr = 10.
    delc = 1.
    delv = 1.
    top = 0.
    botm = [top - delv]
    Lx = (ncol - 1) * delr
    v = 0.24
    prsity = 0.25
    q = v * prsity

    perlen = 2000.
    dt0 = perlen / 10.
    hk = 1.
    laytyp = 1
    rhob = 0.25
    kd = (retardation - 1.) * prsity / rhob

    modelname_mf = 'p01_mf'
    mf = flopy.modflow.Modflow(modelname=modelname_mf, model_ws=model_ws,
                               exe_name=exe_name_mf)
    dis = flopy.modflow.ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol,
                                   delr=delr, delc=delc, top=top, botm=botm,
                                   perlen=perlen)
    ibound = np.ones((nlay, nrow, ncol), dtype=np.int)
    ibound[0, 0, 0] = -1
    ibound[0, 0, -1] = -1
    strt = np.zeros((nlay, nrow, ncol), dtype=np.float)
    h1 = q * Lx
    strt[0, 0, 0] = h1
    bas = flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)
    lpf = flopy.modflow.ModflowLpf(mf, hk=hk, laytyp=laytyp)
    pcg = flopy.modflow.ModflowPcg(mf)
    lmt = flopy.modflow.ModflowLmt(mf)
    mf.write_input()
    mf.run_model(silent=True)

    modelname_mt = 'p01_mt'
    mt = flopy.mt3d.Mt3dms(modelname=modelname_mt, model_ws=model_ws,
                           exe_name=exe_name_mt, modflowmodel=mf)
    c0 = 1.
    icbund = np.ones((nlay, nrow, ncol), dtype=np.int)
    icbund[0, 0, 0] = -1
    sconc = np.zeros((nlay, nrow, ncol), dtype=np.float)
    sconc[0, 0, 0] = c0
    btn = flopy.mt3d.Mt3dBtn(mt, laycon=laytyp, icbund=icbund,
                             prsity=prsity, sconc=sconc, dt0=dt0, ifmtcn=1)
    dceps = 1.e-5
    nplane = 1
    npl = 0
    nph = 4
    npmin = 0
    npmax = 8
    nlsink = nplane
    npsink = nph
    adv = flopy.mt3d.Mt3dAdv(mt, mixelm=mixelm, dceps=dceps, nplane=nplane,
                             npl=npl, nph=nph, npmin=npmin, npmax=npmax,
                             nlsink=nlsink, npsink=npsink, percel=0.5)
    dsp = flopy.mt3d.Mt3dDsp(mt, al=al)
    isothm = 1
    if zeta is not None:
        isothm = 6
    rct = flopy.mt3d.Mt3dRct(mt, isothm=isothm, ireact=1, igetsc=0, rhob=rhob,
                             sp1=kd,
                             sp2=zeta, prsity2=prsity2, rc1=lambda1,
                             rc2=lambda1)
    ssm = flopy.mt3d.Mt3dSsm(mt)
    gcg = flopy.mt3d.Mt3dGcg(mt, mxiter=10)
    mt.write_input()
    fname = os.path.join(model_ws, 'MT3D001.UCN')
    if os.path.isfile(fname):
        os.remove(fname)
    mt.run_model(silent=True)

    fname = os.path.join(model_ws, 'MT3D001.UCN')
    ucnobj = flopy.utils.UcnFile(fname)
    times = ucnobj.get_times()
    conc = ucnobj.get_alldata()

    fname = os.path.join(model_ws, 'MT3D001.OBS')
    if os.path.isfile(fname):
        cvt = mt.load_obs(fname)
    else:
        cvt = None

    fname = os.path.join(model_ws, 'MT3D001.MAS')
    mvt = mt.load_mas(fname)

    return mf, mt, conc, cvt, mvt


def p01mf6(model_ws, al, retardation, lambda1, mixelm,
           zeta=None, prsity2=None):
    name = 'p01'
    nlay, nrow, ncol = 1, 1, 101
    nper = 1
    perlen = [2000.]
    nstp = [10]
    tsmult = [1.]
    steady = [True]
    delr = 10.
    delc = 1.
    delv = 1.
    top = 0.
    botm = [top - delv]
    strt = 1.
    hk = 1.0
    laytyp = 1

    Lx = (ncol - 1) * delr
    v = 0.24
    prsity = 0.25
    q = v * prsity

    rhob = 0.25
    kd = (retardation - 1.) * prsity / rhob

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    ws = model_ws
    exe_name = os.path.abspath(exe_name_mf6)
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name=exe_name,
                                 sim_ws=ws)
    from flopy.mf6.mfbase import VerbosityLevel
    sim.simulation_data.verbosity_level = VerbosityLevel.quiet

    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    # create gwf model
    gwfname = 'gwf_' + name
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True,
                               model_nam_file='{}.nam'.format(gwfname))

    # create iterative model solution and register the gwf model with it
    imsgwf = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                                  outer_hclose=hclose,
                                  outer_maximum=nouter,
                                  under_relaxation='NONE',
                                  inner_maximum=ninner,
                                  inner_hclose=hclose, rcloserecord=rclose,
                                  linear_acceleration='CG',
                                  scaling_method='NONE',
                                  reordering_method='NONE',
                                  relaxation_factor=relax,
                                  fname='{}.ims'.format(gwfname))
    sim.register_ims_package(imsgwf, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm,
                                  idomain=np.ones((nlay, nrow, ncol),
                                                  dtype=np.int),
                                  fname='{}.dis'.format(gwfname))

    # initial conditions
    strt = np.zeros((nlay, nrow, ncol), dtype=np.float)
    h1 = q * Lx
    strt[0, 0, 0] = h1
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                fname='{}.ic'.format(gwfname))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  icelltype=laytyp,
                                  k=hk,
                                  k33=hk, save_specific_discharge=True)

    # chd files
    chdspd = [[(0, 0, 0), h1], [(0, 0, ncol - 1), 0.0]]
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                   maxbound=len(chdspd),
                                                   stress_period_data=chdspd,
                                                   save_flows=False,
                                                   pname='CHD-1')

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.bud'.format(gwfname),
                                head_filerecord='{}.hds'.format(gwfname),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST'),
                                            ('BUDGET', 'LAST')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # create gwt model
    gwtname = 'gwt_' + name
    gwt = flopy.mf6.MFModel(sim, model_type='gwt6', modelname=gwtname,
                            model_nam_file='{}.nam'.format(gwtname))
    gwt.name_file.save_flows = True

    # create iterative model solution and register the gwt model with it
    imsgwt = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                                  outer_hclose=hclose,
                                  outer_maximum=nouter,
                                  under_relaxation='NONE',
                                  inner_maximum=ninner,
                                  inner_hclose=hclose, rcloserecord=rclose,
                                  linear_acceleration='BICGSTAB',
                                  scaling_method='NONE',
                                  reordering_method='NONE',
                                  relaxation_factor=relax,
                                  fname='{}.ims'.format(gwtname))
    sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(gwt, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm,
                                  idomain=1,
                                  fname='{}.dis'.format(gwtname))

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.,
                                fname='{}.ic'.format(gwtname))

    # advection
    if mixelm == 0:
        scheme = 'UPSTREAM'
    elif mixelm == -1:
        scheme = 'TVD'
    else:
        raise Exception()
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme=scheme,
                                  fname='{}.adv'.format(gwtname))

    # dispersion
    dsp = flopy.mf6.ModflowGwtdsp(gwt, alh=al, ath1=0.1)

    # storage
    sto = flopy.mf6.ModflowGwtsto(gwt, porosity=prsity,
                                  fname='{}.sto'.format(gwtname))

    # constant concentration
    c0 = 1.
    cncspd = [[(0, 0, 0), c0]]
    cnc = flopy.mf6.ModflowGwtcnc(gwt, maxbound=len(cncspd),
                                  stress_period_data=cncspd,
                                  save_flows=False,
                                  pname='CNC-1')

    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=[[]],
                                  fname='{}.ssm'.format(gwtname))

    dcy = flopy.mf6.ModflowGwtdcy(gwt, rc=lambda1)

    rct = flopy.mf6.ModflowGwtrct(gwt, sorbtion=True, decayorder='one',
                                  rhob=rhob, distcoef=kd,
                                  rc1=0., rc2=lambda1,
                                  fname='{}.rct'.format(gwtname))

    if zeta is not None:
        imd = flopy.mf6.ModflowGwtimd(gwt, sorbtion=True, decayorder='one',
                                      rhob=rhob, distcoef=kd,
                                      rc1=lambda1, rc2=lambda1,
                                      zetaim=zeta, thetaim=prsity2,
                                      fname='{}.imd'.format(gwtname))

    # output control
    oc = flopy.mf6.ModflowGwtoc(gwt,
                                budget_filerecord='{}.cbc'.format(gwtname),
                                concentration_filerecord='{}.ucn'.format(
                                    gwtname),
                                concentrationprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('CONCENTRATION', 'LAST'),
                                            ('BUDGET', 'LAST')],
                                printrecord=[('CONCENTRATION', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(sim, exgtype='GWF6-GWT6',
                                     exgmnamea=gwfname, exgmnameb=gwtname,
                                     fname='{}.gwfgwt'.format(name))

    sim.write_simulation()
    fname = os.path.join(model_ws, gwtname + '.ucn')
    if os.path.isfile(fname):
        os.remove(fname)
    success, buff = sim.run_simulation(silent=True, report=True)
    if not success:
        print(buff)

    fname = os.path.join(model_ws, gwtname + '.ucn')
    ucnobj = flopy.utils.HeadFile(fname, precision='double',
                                  text='CONCENTRATION')
    times = ucnobj.get_times()
    conc = ucnobj.get_alldata()

    return sim, conc


def test_mt3dmsp01a():

    longitudinal_dispersivity = 0.
    retardation = 1.0
    zero_order_decay = 0.00
    mixelm = 0
    zeta = None
    prsity2 = None

    mf6_ws = os.path.join(testdir, testgroup + 'a')
    sim, conc_mf6 = p01mf6(mf6_ws, longitudinal_dispersivity,
                           retardation, zero_order_decay,
                           mixelm, zeta, prsity2)

    mt3d_ws = os.path.join(mf6_ws, 'mt3d')
    mf, mt, conc_mt3d, cvt, mvt = p01mt3d(mt3d_ws, longitudinal_dispersivity,
                                          retardation, zero_order_decay,
                                          mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3d, conc_mf6)
    assert  np.allclose(conc_mt3d, conc_mf6, atol=1e-4), msg
    return


def test_mt3dmsp01b():

    longitudinal_dispersivity = 10.
    retardation = 1.0
    zero_order_decay = 0.00
    mixelm = 0
    zeta = None
    prsity2 = None

    mf6_ws = os.path.join(testdir, testgroup + 'b')
    sim, conc_mf6 = p01mf6(mf6_ws, longitudinal_dispersivity,
                           retardation, zero_order_decay,
                           mixelm, zeta, prsity2)

    mt3d_ws = os.path.join(mf6_ws, 'mt3d')
    mf, mt, conc_mt3d, cvt, mvt = p01mt3d(mt3d_ws, longitudinal_dispersivity,
                                          retardation, zero_order_decay,
                                          mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3d, conc_mf6)
    assert np.allclose(conc_mt3d, conc_mf6, atol=1e-4), msg
    return


def test_mt3dmsp01c():

    longitudinal_dispersivity = 10.
    retardation = 1.5
    zero_order_decay = 0.00
    mixelm = 0
    zeta = None
    prsity2 = None

    mf6_ws = os.path.join(testdir, testgroup + 'c')
    sim, conc_mf6 = p01mf6(mf6_ws, longitudinal_dispersivity,
                           retardation, zero_order_decay,
                           mixelm, zeta, prsity2)

    mt3d_ws = os.path.join(mf6_ws, 'mt3d')
    mf, mt, conc_mt3d, cvt, mvt = p01mt3d(mt3d_ws, longitudinal_dispersivity,
                                          retardation, zero_order_decay,
                                          mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3d, conc_mf6)
    assert np.allclose(conc_mt3d, conc_mf6, atol=1e-4), msg
    return


def test_mt3dmsp01d():

    longitudinal_dispersivity = 10.
    retardation = 1.5
    zero_order_decay = 0.002
    mixelm = 0
    zeta = None
    prsity2 = None

    mf6_ws = os.path.join(testdir, testgroup + 'd')
    sim, conc_mf6 = p01mf6(mf6_ws, longitudinal_dispersivity,
                           retardation, zero_order_decay,
                           mixelm, zeta, prsity2)

    mt3d_ws = os.path.join(mf6_ws, 'mt3d')
    mf, mt, conc_mt3d, cvt, mvt = p01mt3d(mt3d_ws, longitudinal_dispersivity,
                                          retardation, zero_order_decay,
                                          mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3d, conc_mf6)
    assert np.allclose(conc_mt3d, conc_mf6, atol=1e-4), msg
    return


def test_mt3dmsp01e():

    longitudinal_dispersivity = 10.
    retardation = 1.5
    zero_order_decay = 0.002
    mixelm = 0
    zeta = .1
    prsity2 = 0.05

    mf6_ws = os.path.join(testdir, testgroup + 'e')
    sim, conc_mf6 = p01mf6(mf6_ws, longitudinal_dispersivity,
                           retardation, zero_order_decay,
                           mixelm, zeta, prsity2)

    mt3d_ws = os.path.join(mf6_ws, 'mt3d')
    mf, mt, conc_mt3d, cvt, mvt = p01mt3d(mt3d_ws, longitudinal_dispersivity,
                                          retardation, zero_order_decay,
                                          mixelm, zeta, prsity2)

    msg = 'concentrations not equal {} {}'.format(conc_mt3d, conc_mf6)
    assert np.allclose(conc_mt3d, conc_mf6, atol=1e-1), msg
    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))
    test_mt3dmsp01a()
    test_mt3dmsp01b()
    test_mt3dmsp01c()
    test_mt3dmsp01d()
    test_mt3dmsp01e()
