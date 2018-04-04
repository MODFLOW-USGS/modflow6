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

ex = ['csub_subwt01a', 'csub_subwt01b']
cvopt = [None, 'dewatered']
constantcv = [True, False]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'

# set travis to True when version 1.13.0 is released
travis = [True, True]

# set replace_exe to None to use default executable
replace_exe = None

def build_models():
    pth = os.path.join(ddir, 'ibc01_ibound.ref')
    ib0 = np.genfromtxt(pth)

    nlay, nrow, ncol = 4, ib0.shape[0], ib0.shape[1]
    nper = 3
    perlen = [1., 21915., 21915.]
    nstp = [1, 60, 60]
    tsmult = [1., 1., 1.]
    steady = [True, False, False]
    delr, delc = 2000., 2000.
    top = 150.
    botm = [50., -100., -150., -350.]
    strt = 100.
    hnoflo = 1e30
    hdry = -1e30
    laytyp = [1, 0, 0, 0]
    hk = [4., 4., 1e-2, 4.]
    ss = [0., 1e-6, 1e-6, 1e-6]
    sy = [0.3, 0., 0., 0.]

    w1 = [(0,  0,  7, 2.2000000E+03),
          (0,  1,  4, 2.2000000E+03),
          (0,  1,  7, 2.2000000E+03),
          (0,  1, 11, 2.2000000E+03),
          (0,  2,  3, 2.2000000E+03),
          (0,  3, 11, 2.2000000E+03),
          (0,  4,  2, 2.2000000E+03),
          (0,  4, 12, 2.2000000E+03),
          (0,  5, 13, 2.2000000E+03),
          (0,  6,  1, 2.2000000E+03),
          (0, 13,  1, 2.2000000E+03),
          (0, 13, 13, 2.2000000E+03),
          (0, 15,  2, 2.2000000E+03),
          (0, 15, 12, 2.2000000E+03),
          (0, 16, 12, 2.2000000E+03),
          (0, 17,  3, 2.2000000E+03),
          (0, 17, 11, 2.2000000E+03),
          (0, 18,  6, 2.2000000E+03)]
    w2 = [(0,  0,  7, 2.2000000E+03),
          (0,  1,  4, 2.2000000E+03),
          (0,  1,  7, 2.2000000E+03),
          (0,  1, 11, 2.2000000E+03),
          (0,  2,  3, 2.2000000E+03),
          (0,  3, 11, 2.2000000E+03),
          (0,  4,  2, 2.2000000E+03),
          (0,  4, 12, 2.2000000E+03),
          (0,  5, 13, 2.2000000E+03),
          (0,  6,  1, 2.2000000E+03),
          (0, 13,  1, 2.2000000E+03),
          (0, 13, 13, 2.2000000E+03),
          (0, 15,  2, 2.2000000E+03),
          (0, 15, 12, 2.2000000E+03),
          (0, 16, 12, 2.2000000E+03),
          (0, 17,  3, 2.2000000E+03),
          (0, 17, 11, 2.2000000E+03),
          (0, 18,  6, 2.2000000E+03),
          (1,  8, 9, -7.2000000E+04),
          (3, 11, 6, -7.2000000E+04)]
    wd = {0: w1, 1: w2, 2: w1}

    ws1 = [((0,  0,  7), 2.2000000E+03),
           ((0,  1,  4), 2.2000000E+03),
           ((0,  1,  7), 2.2000000E+03),
           ((0,  1, 11), 2.2000000E+03),
           ((0,  2,  3), 2.2000000E+03),
           ((0,  3, 11), 2.2000000E+03),
           ((0,  4,  2), 2.2000000E+03),
           ((0,  4, 12), 2.2000000E+03),
           ((0,  5, 13), 2.2000000E+03),
           ((0,  6,  1), 2.2000000E+03),
           ((0, 13,  1), 2.2000000E+03),
           ((0, 13, 13), 2.2000000E+03),
           ((0, 15,  2), 2.2000000E+03),
           ((0, 15, 12), 2.2000000E+03),
           ((0, 16, 12), 2.2000000E+03),
           ((0, 17,  3), 2.2000000E+03),
           ((0, 17, 11), 2.2000000E+03),
           ((0, 18,  6), 2.2000000E+03)]
    ws2 = [((0,  0,  7), 2.2000000E+03),
           ((0,  1,  4), 2.2000000E+03),
           ((0,  1,  7), 2.2000000E+03),
           ((0,  1, 11), 2.2000000E+03),
           ((0,  2,  3), 2.2000000E+03),
           ((0,  3, 11), 2.2000000E+03),
           ((0,  4,  2), 2.2000000E+03),
           ((0,  4, 12), 2.2000000E+03),
           ((0,  5, 13), 2.2000000E+03),
           ((0,  6,  1), 2.2000000E+03),
           ((0, 13,  1), 2.2000000E+03),
           ((0, 13, 13), 2.2000000E+03),
           ((0, 15,  2), 2.2000000E+03),
           ((0, 15, 12), 2.2000000E+03),
           ((0, 16, 12), 2.2000000E+03),
           ((0, 17,  3), 2.2000000E+03),
           ((0, 17, 11), 2.2000000E+03),
           ((0, 18,  6), 2.2000000E+03),
           ((1,  8,  9), -7.2000000E+04),
           ((3, 11,  6), -7.2000000E+04)]
    wd6 = {0: ws1, 1: ws2, 2: ws1}

    chd1 = [(0, 19,  7, 100.00000, 100.00000),
            (0, 19,  8, 100.00000, 100.00000),
            (1, 19,  7, 100.00000, 100.00000),
            (1, 19,  8, 100.00000, 100.00000),
            (2, 19,  7, 100.00000, 100.00000),
            (2, 19,  8, 100.00000, 100.00000),
            (3, 19,  7, 100.00000, 100.00000),
            (3, 19,  8, 100.00000, 100.00000)]
    cd = {0: chd1}

    chd6 = [((0, 19,  7), 100.00000),
            ((0, 19,  8), 100.00000),
            ((1, 19,  7), 100.00000),
            ((1, 19,  8), 100.00000),
            ((2, 19,  7), 100.00000),
            ((2, 19,  8), 100.00000),
            ((3, 19,  7), 100.00000),
            ((3, 19,  8), 100.00000)]
    cd6 = {0: chd6}

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 0.01, 1.

    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

    # this used to work
    #ib = np.zeros((nlay, nrow, ncol), dtype=np.int)
    #for k in range(nlay):
    #    ib[k, :, :] = ib0.copy()
    ib = []
    for k in range(nlay):
        ib.append(ib0.astype(np.int).copy())


    # subwt data
    cc = 0.25
    cr = 0.01
    void = 0.82
    theta = void / (1. + void)
    kv = 999.
    sgm = 1.7
    sgs = 2.0
    ini_stress = 15.0
    delay_flag = 0
    thick = [45., 70., 50., 90.]

    swt6 = []
    ibcno = 0
    for k in range(nlay):
        for i in range(nrow):
            for j in range(ncol):
                iactive = 0
                if ib0[i, j] > 0:
                    iactive = 1
                if i == 19 and (j == 7 or j == 8):
                    iactive = 0
                if iactive > 0:
                    ibcno += 1
                    d = [ibcno, (k, i, j), 'nodelay', ini_stress, thick[k],
                         1., cc, cr, theta,
                         kv, 999.]
                    swt6.append(d)
    ds16 = [0, 2052, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ds17 = [1, 10000, 1, 10000, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    for idx, dir in enumerate(exdirs):
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
        gwf = flopy.mf6.ModflowGwf(sim, modelname=name)

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
                                      top=top, botm=botm,
                                      idomain=ib,
                                      fname='{}.dis'.format(name))


        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                    fname='{}.ic'.format(name))

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False,
                                      cvoptions=cvopt[idx],
                                      icelltype=laytyp,
                                      k=hk,
                                      k33=hk)
        # storage
        sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False, iconvert=laytyp,
                                      ss=ss, sy=sy,
                                      steady_state={0: True},
                                      transient={1: True})

        # chd files
        chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf,
                                                       maxbound=len(chd6),
                                                       stress_period_data=cd6,
                                                       save_flows=False)

        # wel files
        wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
                                      maxbound=len(ws2),
                                      stress_period_data=wd6,
                                      save_flows=False)

        # ibc files
        ibc = flopy.mf6.ModflowGwfcsub(gwf, interbed_stress_offset=True,
                                       compression_indices=True,
                                       geo_stress_offset=True,
                                       ninterbeds=len(swt6),
                                       sgs=sgs, sgm=sgm,
                                       beta=0., ske_cr=0.00,
                                       packagedata=swt6,
                                       sig0={0: [0., 0., 0., 0.]})

        # output control
        oc = flopy.mf6.ModflowGwfoc(gwf,
                                    budget_filerecord='{}.cbc'.format(name),
                                    head_filerecord='{}.hds'.format(name),
                                    headprintrecord=[
                                        ('COLUMNS', 10, 'WIDTH', 15,
                                         'DIGITS', 6, 'GENERAL')],
                                    saverecord=[('HEAD', 'LAST')],
                                    printrecord=[('HEAD', 'LAST'),
                                                 ('BUDGET', 'LAST')])

        # write MODFLOW 6 files
        sim.write_simulation()

        # build MODFLOW-2005 files
        ws = os.path.join(dir, 'mf2005')
        mc = flopy.modflow.Modflow(name, model_ws=ws)
        dis = flopy.modflow.ModflowDis(mc, nlay=nlay, nrow=nrow, ncol=ncol,
                                       nper=nper, perlen=perlen, nstp=nstp,
                                       tsmult=tsmult, steady=steady, delr=delr,
                                       delc=delc, top=top, botm=botm)
        bas = flopy.modflow.ModflowBas(mc, ibound=ib, strt=strt, hnoflo=hnoflo)
        lpf = flopy.modflow.ModflowLpf(mc, laytyp=laytyp, hk=hk, vka=hk, ss=ss,
                                       sy=sy, constantcv=constantcv[idx],
                                       hdry=hdry)
        chd = flopy.modflow.ModflowChd(mc, stress_period_data=cd)
        wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
        swt = flopy.modflow.ModflowSwt(mc, iswtoc=1, nsystm=4, ithk=1, ivoid=1,
                                       istpcs=1, lnwt=[0, 1, 2, 3],
                                       cc=cc, cr=cr, thick=thick,
                                       void=void, pcsoff=ini_stress, sgs=sgs,
                                       gl0=0., ids16=ds16, ids17=ds17)
        oc = flopy.modflow.ModflowOc(mc, stress_period_data=None)
        pcg = flopy.modflow.ModflowPcg(mc, mxiter=nouter, iter1=ninner,
                                       hclose=hclose, rclose=rclose,
                                       relax=relax)
        mc.write_input()

    return

# - No need to change any code below
def test_mf6model():
    # determine if running on Travis
    is_travis = 'TRAVIS' in os.environ
    r_exe = None
    if not is_travis:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        if is_travis and not travis[idx]:
            continue
        yield test.run_mf6, Simulation(dir, exe_dict=r_exe)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for dir in exdirs:
        sim = Simulation(dir, exe_dict=replace_exe)
        test.run_mf6(sim)

    return


# use python testmf6_csub_sub02.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()

