# test reading of binary initial heads (float) and also binary icelltype (int).
# 1. Have binary data in a separate record for each layer
# 2. Have binary data in a single record for all layers

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

ex = ['binary01', 'binary02']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def build_models():

    nlay, nrow, ncol = 5, 6, 7
    nper = 1
    perlen = 1.
    nstp = 1
    tsmult = 1.
    steady = [True]
    lenx = 300.
    delr = delc = lenx / float(nrow)
    botm = np.linspace(-1., -5., 5)
    hnoflo = 1e30
    hdry = -1e30
    hk = 1.

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-3, 1.

    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((perlen, nstp, tsmult))

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
                                     nper=nper, perioddata=tdis_rc)

        # create gwf model
        gwf = flopy.mf6.MFModel(sim, model_type='gwf6', modelname=name,
                                model_nam_file='{}.nam'.format(name))
        gwf.name_file.newtonoptions = None

        # create iterative model solution and register the gwf model with it
        ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                                   outer_hclose=hclose,
                                   outer_maximum=nouter,
                                   under_relaxation='NONE',
                                   inner_maximum=ninner,
                                   inner_hclose=hclose, rcloserecord=rclose,
                                   linear_acceleration='CG',
                                   scaling_method='NONE',
                                   reordering_method='NONE',
                                   relaxation_factor=relax)
        sim.register_ims_package(ims, [gwf.name])

        dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                      delr=delr, delc=delc,
                                      top=0., botm=botm,
                                      idomain=1,
                                      fname='{}.dis'.format(name))

        # initial conditions
        # write initial heads to binary file
        fname = 'ic.strt.bin'
        pth = os.path.join(exdirs[idx], fname)
        f = open(pth, 'wb')
        if idx == 0:
            for k in range(nlay):
                header = flopy.utils.BinaryHeader.create(bintype='HEAD',
                                                         precision='double',
                                                         text='HEAD',
                                                         nrow=nrow,
                                                         ncol=ncol,
                                                         ilay=k+1, pertim=1.0,
                                                         totim=1.0, kstp=1,
                                                         kper=1)
                flopy.utils.Util2d.write_bin((nrow, ncol), f,
                                             np.ones((nrow, ncol),
                                                     dtype=np.float),
                                             header_data=header)
        elif idx == 1:
            header = flopy.utils.BinaryHeader.create(bintype='HEAD',
                                                     precision='double',
                                                     text='HEAD', nrow=1,
                                                     ncol=ncol*nrow*nlay,
                                                     ilay=1, pertim=1.0,
                                                     totim=1.0, kstp=1, kper=1)
            flopy.utils.Util2d.write_bin((nrow, ncol), f,
                                         np.ones((nrow*ncol*nlay),
                                                 dtype=np.float),
                                         header_data=header)
        f.close()
        strt = {'factor': 1., 'filename': fname,
                'data': None, 'binary': True, 'iprn': 1}
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                    fname='{}.ic'.format(name))

        # node property flow
        # write icelltype to binary file
        fname = 'npf.icelltype.bin'
        pth = os.path.join(exdirs[idx], fname)
        f = open(pth, 'wb')
        if idx == 0:
            for k in range(nlay):
                header = flopy.utils.BinaryHeader.create(bintype='head',
                                                         text='ICELLTYPE',
                                                         precision='double',
                                                         nrow=nrow,
                                                         ncol=ncol,
                                                         ilay=k+1, pertim=1.0,
                                                         totim=1.0, kstp=1,
                                                         kper=1)
                header.tofile(f)
                flopy.utils.Util2d.write_bin((nrow, ncol), f,
                                             np.ones((nrow, ncol),
                                                     dtype=np.int32),
                                             header_data=header)
        elif idx == 1:
            header = flopy.utils.BinaryHeader.create(bintype='head',
                                                     text='ICELLTYPE',
                                                     precision='double',
                                                     nrow=1,
                                                     ncol=ncol*nrow*nlay,
                                                     ilay=1, pertim=1.0,
                                                     totim=1.0, kstp=1, kper=1)
            header.tofile(f)
            flopy.utils.Util2d.write_bin((nrow, ncol), f,
                                         np.ones((nrow*ncol*nlay),
                                                 dtype=np.int32),
                                         header_data=header)
        f.close()
        icelltype = {'factor': 1., 'filename': fname,
                     'data': None, 'binary': True, 'iprn': 1}
        npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True,
                                      icelltype=icelltype,
                                      k=hk,
                                      k33=hk,
                                      fname='{}.npf'.format(name))

        # chd files
        chdlist0 = []
        chdlist0.append([(0, 0, 0), 1.])
        chdlist0.append([(nlay-1, nrow-1, ncol-1), 0.])

        chdspdict = {0: chdlist0}
        chd = flopy.mf6.ModflowGwfchd(gwf,
                                      stress_period_data=chdspdict,
                                      save_flows=False,
                                      fname='{}.chd'.format(name))

        # output control
        oc = flopy.mf6.ModflowGwfoc(gwf,
                                    budget_filerecord='{}.cbc'.format(name),
                                    head_filerecord='{}.hds'.format(name),
                                    headprintrecord=[
                                        ('COLUMNS', 10, 'WIDTH', 15,
                                         'DIGITS', 6, 'GENERAL')],
                                    saverecord=[('HEAD', 'ALL')],
                                    printrecord=[('HEAD', 'ALL'),
                                                 ('BUDGET', 'ALL')],
                                    fname='{}.oc'.format(name))

        # write MODFLOW 6 files
        sim.write_simulation()


    return

# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
