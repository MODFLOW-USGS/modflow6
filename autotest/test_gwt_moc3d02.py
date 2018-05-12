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

ex = ['moc3d02a', 'moc3d02b']
xt3d = [None, True]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def get_model(idx, dir):
    nlay, nrow, ncol = 40, 12, 30
    nper = 1
    perlen = [400]
    nstp = [400]
    tsmult = [1.]
    steady = [True]
    delr = 3.0
    delc = 0.5
    top = 0.
    delz = 0.05
    botm =  np.arange(-delz, -nlay*delz - delz, -delz)
    strt = 0.
    hnoflo = 1e30
    hdry = -1e30
    hk = 0.0125 / delz
    laytyp = 0
    diffc = 0.
    alphal = 0.6
    alphath = 0.03
    alphatv = 0.006
    porosity = 0.25
    #ss = 0.
    #sy = 0.1

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-8, 1e-6, 1.

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

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
                                  idomain=np.ones((nlay, nrow, ncol), dtype=np.int),
                                  fname='{}.dis'.format(gwfname))

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                fname='{}.ic'.format(gwfname))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                  save_specific_discharge=True,
                                  icelltype=laytyp,
                                  k=hk,
                                  k33=hk)

    # chd files
    chdlist = []
    j = ncol - 1
    for k in range(nlay):
        for i in range(nrow):
            chdlist.append([(k, i, j), 0.])
    chd = flopy.mf6.ModflowGwfchd(gwf,
                                  stress_period_data=chdlist,
                                  save_flows=False,
                                  pname='CHD-1')

    # wel files
    wellist = []
    j = 0
    qwell = 0.1 * delz * delc * porosity
    for k in range(nlay):
        for i in range(nrow):
            wellist.append([(k, i, j), qwell, 0.])
    wellist.append([(0, 0, 7), 1.e-6, 2.5e6])  # source well

    wel = flopy.mf6.ModflowGwfwel(gwf,
                                  print_input=True,
                                  print_flows=True,
                                  stress_period_data=wellist,
                                  save_flows=False,
                                  auxiliary='CONCENTRATION',
                                  pname='WEL-1')

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(gwfname),
                                head_filerecord='{}.hds'.format(gwfname),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # create gwt model
    gwtname = 'gwt_' + name
    gwt = flopy.mf6.MFModel(sim, model_type='gwt6', modelname=gwtname,
                            model_nam_file='{}.nam'.format(gwtname))

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
    strt = np.zeros((nlay, nrow, ncol))
    strt[0, 0, 0] = 0.
    ic = flopy.mf6.ModflowGwtic(gwt, strt=strt,
                                fname='{}.ic'.format(gwtname))

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme='TVD',
                                  fname='{}.adv'.format(gwtname))

    # dispersion
    dsp = flopy.mf6.ModflowGwtdsp(gwt, xt3d=xt3d[idx], diffc=diffc,
                                  alh=alphal, alv=alphal,
                                  ath=alphath, atv=alphatv,
                                  fname='{}.dsp'.format(gwtname))

    # storage
    sto = flopy.mf6.ModflowGwtsto(gwt, porosity=porosity,
                                fname='{}.sto'.format(gwtname))

    # sources
    sourcerecarray = [('WEL-1', 1, 'CONCENTRATION')]
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray,
                                fname='{}.ssm'.format(gwtname))

    # output control
    oc = flopy.mf6.ModflowGwtoc(gwt,
                                budget_filerecord='{}.cbc'.format(gwtname),
                                concentration_filerecord='{}.ucn'.format(gwtname),
                                concentrationprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('CONCENTRATION', 'ALL')],
                                printrecord=[('CONCENTRATION', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(sim, exgtype='GWF6-GWT6',
                                     exgmnamea=gwfname, exgmnameb=gwtname,
                                     fname='{}.gwfgwt'.format(name))

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def eval_transport(sim):
    print('evaluating transport...')

    name = ex[sim.idxsim]
    gwtname = 'gwt_' + name

    fpth = os.path.join(sim.simpath, '{}.ucn'.format(gwtname))
    try:
        cobj = flopy.utils.HeadFile(fpth, precision='double',
                                    text='CONCENTRATION')
        times = cobj.get_times()
        t = times[-1]
        csim = cobj.get_data(totim=t)
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    cres = np.array([
       27.81683893, 27.74118085, 27.59074397, 27.36730147, 27.07355559,
       26.71321033, 26.2911281 , 25.81372488, 25.28373078, 24.70376874,
       24.07707513, 23.40732213, 22.69843933, 21.95594778, 21.18681381,
       20.39770536, 19.59505602, 18.78507897, 17.97370171, 17.16651555,
       16.36870142, 15.58507333, 14.82012258, 14.07804048, 13.36269606,
       12.67762756, 12.02604254, 11.41082379, 10.83453961, 10.29945756,
        9.80756102,  9.36056761,  8.95994886,  8.60695017,  8.30261058,
        8.04778147,  7.84314372,  7.68922285,  7.5864015 ,  7.53492928])

    csim = csim[:, 2, 12]
    assert np.allclose(cres, csim), 'simulated concentrations do not match with known solution.'

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_transport, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
