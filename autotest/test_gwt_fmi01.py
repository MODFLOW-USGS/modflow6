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
from binary_file_writer import write_head, write_budget, uniform_flow_field

ex = ['fmi01a_fc', ]
xt3d = [False, True]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def get_model(idx, dir):
    nlay, nrow, ncol = 1, 1, 3
    nper = 1
    perlen = [1.0]
    nstp = [1]
    tsmult = [1.]
    steady = [True]
    delr = 1.
    delc = 1.
    top = 1.
    laytyp = 0
    ss = 0.
    sy = 0.1
    botm = [0.]
    strt = 1.
    hnoflo = 1e30
    hdry = -1e30
    hk = 1.0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.

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
                                  filename='{}.ims'.format(gwtname))
    sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(gwt, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm,
                                  idomain=1,
                                  filename='{}.dis'.format(gwtname))

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=10.,
                                filename='{}.ic'.format(gwtname))

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt)

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.1)

    # output control
    oc = flopy.mf6.ModflowGwtoc(gwt,
                                budget_filerecord='{}.cbc'.format(gwtname),
                                concentration_filerecord='{}.ucn'.format(gwtname),
                                concentrationprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('CONCENTRATION', 'LAST'),
                                            ('BUDGET', 'LAST')],
                                printrecord=[('CONCENTRATION', 'LAST'),
                                             ('BUDGET', 'LAST')])

    # create a heads file with head equal top
    fname = os.path.join(ws, 'myhead.hds')
    with open(fname, 'wb') as fbin:
        for kstp in range(nstp[0]):
            write_head(fbin, top * np.ones((nrow, ncol)), kstp=kstp + 1)

    # create a budget file
    qx = 0.
    qy = 0.
    qz = 0.
    shape = (nlay, nrow, ncol)
    spdis, flowja = uniform_flow_field(qx, qy, qz, shape)
    dt = np.dtype([('ID1', np.int32),
                   ('ID2', np.int32),
                   ('FLOW', np.float64),
                   ('CONCENTRATION', np.float64),
                   ])
    print(flowja)
    flowja = np.array([ 0., 0.01,
                        0.,  0.,  -0.01,
                        0.,  0.01])

    fname = os.path.join(ws, 'mybudget.bud')
    with open(fname, 'wb') as fbin:
        for kstp in range(nstp[0]):
            write_budget(fbin, flowja, kstp=kstp + 1)
            write_budget(fbin, spdis, text='      DATA-SPDIS', imeth=6,
                         kstp=kstp + 1)
    fbin.close()

    # flow model interface
    fmi = flopy.mf6.ModflowGwtfmi(gwt, flow_imbalance_correction=True,
                                  gwfbudget_filerecord='mybudget.bud',
                                  gwfhead_filerecord='myhead.hds')

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

    # This is the answer to this problem.  Concentration should not change
    cres = [[[10, 10, 10]]]
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
