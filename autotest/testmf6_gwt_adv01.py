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

ex = ['adv01']
top = [1.]
laytyp = [0]
ss = [0.]
sy = [0.1]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def build_models():
    nlay, nrow, ncol = 1, 1, 100
    nper = 1
    perlen = [5.0]
    nstp = [200]
    tsmult = [1.]
    steady = [True]
    delr = 1.
    delc = 1.
    botm = [0.]
    strt = 1.
    hnoflo = 1e30
    hdry = -1e30
    hk = 1.0

    c = {0: [[(0, 0, 99), 0.0000000]]}
    w = {0: [[(0, 0, 0), 1.0, 1.0]]}

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.

    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

    for idx, dir in enumerate(exdirs):
        name = ex[idx]

        # build MODFLOW 6 files
        ws = dir
        sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                     exe_name='mf6',
                                     sim_ws=ws,
                                     sim_tdis_file='simulation.tdis')
        # create tdis package
        tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                     nper=nper, tdisrecarray=tdis_rc)

        # create gwf model
        gwfname = 'gwf_' + name
        gwf = flopy.mf6.MFModel(sim, model_type='gwf6', model_name=gwfname,
                                model_nam_file='{}.nam'.format(gwfname),
                                ims_file_name='{}.ims'.format(gwfname))

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
                                      top=top[idx], botm=botm,
                                      idomain=np.ones((nlay, nrow, ncol), dtype=np.int),
                                      fname='{}.dis'.format(gwfname))

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                    fname='{}.ic'.format(gwfname))

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                      icelltype=laytyp[idx],
                                      k=hk,
                                      k33=hk)
        # storage
        #sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False,
        #                              iconvert=laytyp[idx],
        #                              ss=ss[idx], sy=sy[idx],
        #                              steady_state={0: True, 2: True},
        #                              transient={1: True})

        # chd files
        chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                       maxbound=len(c),
                                                       periodrecarray=c,
                                                       save_flows=False,
                                                       pname='CHD-1')

        # wel files
        wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
                                      maxbound=len(w),
                                      periodrecarray=w,
                                      save_flows=False,
                                      auxiliary='CONCENTRATION', pname='WEL-1')

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
        gwt = flopy.mf6.MFModel(sim, model_type='gwt6', model_name=gwtname,
                                model_nam_file='{}.nam'.format(gwtname),
                                ims_file_name='{}.ims'.format(gwtname))

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
                                      top=top[idx], botm=botm,
                                      idomain=1,
                                      fname='{}.dis'.format(gwtname))

        # initial conditions
        ic = flopy.mf6.ModflowGwtic(gwt, strt=0.,
                                    fname='{}.ic'.format(gwtname))

        # advection
        adv = flopy.mf6.ModflowGwtadv(gwt, scheme='UPSTREAM',
                                    fname='{}.adv'.format(gwtname))

        # storage
        sto = flopy.mf6.ModflowGwtsto(gwt, porosity=0.1,
                                    fname='{}.sto'.format(gwtname))

        # sources
        sourcerecarray = [['WEL-1, 1, CONCENTRATION']]
        ssm = flopy.mf6.ModflowGwtssm(gwt, sourcerecarray=sourcerecarray,
                                    fname='{}.ssm'.format(gwtname))

        # output control
        oc = flopy.mf6.ModflowGwtoc(gwt,
                                    budget_filerecord='{}.cbc'.format(gwtname),
                                    concentration_filerecord='{}.ucn'.format(gwtname),
                                    concentrationprintrecord=[
                                        ('COLUMNS', 10, 'WIDTH', 15,
                                         'DIGITS', 6, 'GENERAL')],
                                    saverecord=[('CONCENTRATION', 'LAST')],
                                    printrecord=[('CONCENTRATION', 'LAST'),
                                                 ('BUDGET', 'LAST')])

        # GWF GWT exchange
        gwfgwt = flopy.mf6.ModflowGwfgwt(sim, exgtype='GWF6-GWT6',
                                         exgmnamea=gwfname, exgmnameb=gwtname,
                                         fname='{}.gwfgwt'.format(name))

        # write MODFLOW 6 files
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
        conc = cobj.get_data()
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # This is the answer to this problem.  These concentrations are for
    # time step 200.
    cres = [[[1.00000000e+00, 1.00000000e+00, 1.00000000e+00, 1.00000000e+00,
              1.00000000e+00, 1.00000000e+00, 1.00000000e+00, 1.00000000e+00,
              1.00000000e+00, 1.00000000e+00, 1.00000000e+00, 9.99999999e-01,
              9.99999997e-01, 9.99999991e-01, 9.99999971e-01, 9.99999914e-01,
              9.99999761e-01, 9.99999372e-01, 9.99998435e-01, 9.99996286e-01,
              9.99991577e-01, 9.99981712e-01, 9.99961893e-01, 9.99923632e-01,
              9.99852532e-01, 9.99725120e-01, 9.99504599e-01, 9.99135431e-01,
              9.98536850e-01, 9.97595635e-01, 9.96158712e-01, 9.94026505e-01,
              9.90948130e-01, 9.86619748e-01, 9.80687319e-01, 9.72754814e-01,
              9.62398489e-01, 9.49187176e-01, 9.32707801e-01, 9.12594513e-01,
              8.88559134e-01, 8.60420154e-01, 8.28127324e-01, 7.91779115e-01,
              7.51630867e-01, 7.08092322e-01, 6.61714306e-01, 6.13165405e-01,
              5.63200494e-01, 5.12623768e-01, 4.62249349e-01, 4.12862664e-01,
              3.65185517e-01, 3.19847250e-01, 2.77363614e-01, 2.38124183e-01,
              2.02388273e-01, 1.70288648e-01, 1.41841739e-01, 1.16962748e-01,
              9.54838854e-02, 7.71740354e-02, 6.17583229e-02, 4.89363652e-02,
              3.83983188e-02, 2.98381826e-02, 2.29641338e-02, 1.75059339e-02,
              1.32196416e-02, 9.89000005e-03, 7.33093269e-03, 5.38459977e-03,
              3.91944360e-03, 2.82760119e-03, 2.02199855e-03, 1.43337156e-03,
              1.00739149e-03, 7.02013580e-04, 4.85116958e-04, 3.32465664e-04,
              2.25991387e-04, 1.52379541e-04, 1.01928496e-04, 6.76460984e-05,
              4.45462926e-05, 2.91101871e-05, 1.88792800e-05, 1.21527525e-05,
              7.76522212e-06, 4.92565188e-06, 3.10201677e-06, 1.93969988e-06,
              1.20440812e-06, 7.42676511e-07, 4.54831064e-07, 2.76669882e-07,
              1.67174989e-07, 1.00349240e-07, 5.98446532e-08, 3.54600737e-08]]]
    cres = np.array(cres)
    assert np.allclose(cres, conc), 'simulated concentrations do not match with known solution.'

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
