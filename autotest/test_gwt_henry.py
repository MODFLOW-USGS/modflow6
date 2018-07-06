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

ex = ['henry01',  # This is for flow and transport in separate matrix solutions
      'henry02']  # This is for flow and transport in the same matrix solution
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))


def get_model(idx, dir):

    lx = 2.
    lz = 1.

    nlay = 10
    nrow = 1
    ncol = 20
    nper = 1
    delr = lx / ncol
    delc = 1.
    top = 1.
    delz = lz / nlay
    botm = list(top - np.arange(delz, nlay * delz + delz, delz))

    perlen = [0.5]
    nstp = [500]
    tsmult = [1.]
    steady = [True]
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    # This should be changed to one true and one false after scott fixes
    # flopy.
    single_matrix_list = [False, False]
    single_matrix = single_matrix_list[idx]
    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-10, 1e-6, 0.97

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
    gwtname = 'gwt_' + name

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
                                  fname='{}.ims'.format(gwfname))
    if single_matrix:
        sim.register_ims_package(imsgwf, [gwfname, gwtname])
    else:
        sim.register_ims_package(imsgwf, [gwfname])

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=1.)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, xt3doptions=False,
                                  save_flows=True,
                                  save_specific_discharge=True,
                                  icelltype=0,
                                  k=864.)

    buy = flopy.mf6.ModflowGwfbuy(gwf)

    def chd_value(k):
        depth = k * delz + 0.5 * delz
        hf = top + 0.025 * depth
        return hf

    # chd files
    chdlist1 = []
    for k in range(nlay):
        chdlist1.append([(k, 0, ncol - 1), chd_value(k), 35.])
    chd1 = flopy.mf6.ModflowGwfchd(gwf,
                                   stress_period_data=chdlist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='CHD-1',
                                   auxiliary='CONCENTRATION',
                                   fname='{}.chd'.format(gwfname))

    wellist1 = []
    qwell = 5.7024 / nlay
    for k in range(nlay):
        wellist1.append([(k, 0, 0), qwell, 0.])
    wel1 = flopy.mf6.ModflowGwfwel(gwf,
                                   stress_period_data=wellist1,
                                   print_input=True,
                                   print_flows=True,
                                   save_flows=False,
                                   pname='WEL-1',
                                   auxiliary='CONCENTRATION',
                                   fname='{}.wel'.format(gwfname))

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(gwfname),
                                head_filerecord='{}.hds'.format(gwfname),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'LAST'),
                                            ('BUDGET', 'LAST')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # create gwt model
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
                                      fname='{}.ims'.format(gwtname))
        sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(gwt, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm)

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=35.,
                                fname='{}.ic'.format(gwtname))

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme='UPSTREAM',
                                  fname='{}.adv'.format(gwtname))

    # dispersion
    diffc = 0.57024
    dsp = flopy.mf6.ModflowGwtdsp(gwt, xt3d=False, diffc=diffc,
                                  # alh=0., alv=0., ath=0., atv=0.,
                                  fname='{}.dsp'.format(gwtname))

    # storage
    porosity = 0.35
    sto = flopy.mf6.ModflowGwtsto(gwt, porosity=porosity,
                                  fname='{}.sto'.format(gwtname))

    # sources
    sourcerecarray = [('CHD-1', 1, 'CONCENTRATION'),
                      ('WEL-1', 1, 'CONCENTRATION')]
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray,
                                  fname='{}.ssm'.format(gwtname))

    # output control
    oc = flopy.mf6.ModflowGwtoc(gwt,
                                budget_filerecord='{}.cbc'.format(gwtname),
                                concentration_filerecord='{}.ucn'.format(
                                    gwtname),
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
        conc = cobj.get_data()
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # This is the answer to this problem.  These concentrations are for
    # time step 500 and only for the bottom layer.
    cres = [[2.89573493e-05, 1.14446653e-04, 4.51581911e-04, 1.76640638e-03,
             6.82342381e-03, 2.59046347e-02, 9.59028163e-02, 3.41705012e-01,
             1.14554311e+00, 3.47615268e+00, 8.98354607e+00, 1.51879954e+01,
             2.06578219e+01, 2.51309261e+01, 2.85181814e+01, 3.09868580e+01,
             3.26940512e+01, 3.37932558e+01, 3.44410870e+01, 3.47819736e+01]]

    cres = np.array(cres)
    assert np.allclose(cres, conc[-1, :, :]), ('simulated concentrations '
        'do not match with known solution.', cres, conc[-1, :, :])

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
