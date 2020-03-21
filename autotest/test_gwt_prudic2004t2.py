# Second problem described by Prudic et al (2004)
# This problem involves transport through an aquifers, lakes and streams.
# It requires the use of the Water Mover Package to send water from a stream,
# into a lake, and then back into another stream. Solute is also transport
# through the system.

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

ex = ['prudic2004t2']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))


def get_model(idx, dir):

    ws = dir
    data_ws = './data/prudic2004test2/'
    name = ex[idx]
    gwfname = 'gwf_' + name
    gwtname = 'gwt_' + name
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6', sim_ws=ws, continue_=True)

    tdis_rc = [(1., 1, 1.), (365.25 * 25, 25, 1.)]
    nper = len(tdis_rc)
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname)

    # ims
    hclose = 0.01
    rclose = 0.1
    nouter = 1000
    ninner = 100
    relax = 0.99
    imsgwf = flopy.mf6.ModflowIms(sim, print_option='ALL',
                                  outer_hclose=hclose,
                                  outer_maximum=nouter,
                                  under_relaxation='NONE',
                                  inner_maximum=ninner,
                                  inner_hclose=hclose,
                                  rcloserecord=rclose,
                                  linear_acceleration='CG',
                                  scaling_method='NONE',
                                  reordering_method='NONE',
                                  relaxation_factor=relax,
                                  filename='{}.ims'.format(gwfname))

    nlay = 8
    nrow = 36
    ncol = 23
    delr = 405.665
    delc = 403.717
    top = 100.
    fname = os.path.join(data_ws, 'bot1.dat')
    bot0 = np.loadtxt(fname)
    botm = [bot0] + [bot0 - (15. * k) for k in range(1, nlay)]
    fname = os.path.join(data_ws, 'idomain1.dat')
    idomain0 = np.loadtxt(fname, dtype=np.int)
    idomain = nlay * [idomain0]
    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm, idomain=idomain)
    idomain = dis.idomain.array

    ic = flopy.mf6.ModflowGwfic(gwf, strt=50.)

    npf = flopy.mf6.ModflowGwfnpf(gwf, xt3doptions=False,
                                  save_flows=True,
                                  save_specific_discharge=True,
                                  icelltype=[1] + 7 * [0],
                                  k=250., k33=125.)

    sto_on = False
    if sto_on:
        sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=True,
                                      iconvert=[1] + 7 * [0],
                                      ss=1.e-5, sy=0.3,
                                      steady_state={0: True},
                                      transient={1: False})

    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.bud'.format(gwfname),
                                head_filerecord='{}.hds'.format(gwfname),
                                headprintrecord=[
                                    ('COLUMNS', ncol, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'), ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'ALL'),
                                             ('BUDGET', 'ALL')])

    rch_on = True
    if rch_on:
        rch = flopy.mf6.ModflowGwfrcha(gwf, recharge={0: 4.79e-3},
                                       pname='RCH-1')

    chdlist = []
    fname = os.path.join(data_ws, 'chd.dat')
    for line in open(fname, 'r').readlines():
        ll = line.strip().split()
        if len(ll) == 4:
            k, i, j, hd = ll
            chdlist.append([(int(k) - 1, int(i) - 1, int(j) - 1,), float(hd)])
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chdlist,
                                  pname='CHD-1')

    rivlist = []
    fname = os.path.join(data_ws, 'riv.dat')
    for line in open(fname, 'r').readlines():
        ll = line.strip().split()
        if len(ll) == 7:
            k, i, j, s, c, rb, bn = ll
            rivlist.append(
                [(int(k) - 1, int(i) - 1, int(j) - 1,), float(s), float(c),
                 float(rb), bn])
    rivra = \
    flopy.mf6.ModflowGwfriv.stress_period_data.empty(gwf, maxbound=len(rivlist),
                                                     boundnames=True)[0]
    for i, t in enumerate(rivlist):
        rivra[i] = tuple(t)
    sfrpd = np.genfromtxt(data_ws + 'sfr-packagedata.dat', names=True)
    sfrpackagedata = flopy.mf6.ModflowGwfsfr.packagedata.empty(gwf,
                                                               boundnames=True,
                                                               maxbound=
                                                               sfrpd.shape[0])
    for name in sfrpackagedata.dtype.names:
        if name in rivra.dtype.names:
            sfrpackagedata[name] = rivra[name]
    for name in sfrpackagedata.dtype.names:
        if name in sfrpd.dtype.names:
            sfrpackagedata[name] = sfrpd[name]
    sfrpackagedata['boundname'] = rivra['boundname']
    with open(data_ws + 'sfr-connectiondata.dat') as f:
        lines = f.readlines()
    sfrconnectiondata = []
    for line in lines:
        t = line.split()
        c = []
        for v in t:
            i = int(v)
            c.append(i)
        sfrconnectiondata.append(c)
    sfrperioddata = {0: [[0, 'inflow', 86400], [18, 'inflow', 8640.]]}

    sfr_on = True
    if sfr_on:
        sfr = flopy.mf6.ModflowGwfsfr(gwf,
                                      print_stage=True,
                                      print_flows=True,
                                      stage_filerecord=gwfname + '.sfr.bin',
                                      budget_filerecord=gwfname + '.sfr.bud',
                                      mover=True, pname='SFR-1',
                                      unit_conversion=128390.00,
                                      boundnames=True, nreaches=len(rivlist),
                                      packagedata=sfrpackagedata,
                                      connectiondata=sfrconnectiondata,
                                      perioddata=sfrperioddata)

    fname = os.path.join(data_ws, 'lakibd.dat')
    lakibd = np.loadtxt(fname, dtype=np.int)
    lakeconnectiondata = []
    nlakecon = [0, 0]
    lak_leakance = 1.
    for i in range(nrow):
        for j in range(ncol):
            if lakibd[i, j] == 0:
                continue
            else:
                ilak = lakibd[i, j] - 1
                # back
                if i > 0:
                    if lakibd[i - 1, j] == 0 and idomain[0, i - 1, j]:
                        h = [ilak, nlakecon[ilak], (0, i - 1, j), 'horizontal',
                             lak_leakance, 0.0, 0.0, delc / 2., delr]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # left
                if j > 0:
                    if lakibd[i, j - 1] and idomain[0, i, j - 1] == 0:
                        h = [ilak, nlakecon[ilak], (0, i, j - 1), 'horizontal',
                             lak_leakance, 0.0, 0.0, delr / 2., delc]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # right
                if j < ncol - 1:
                    if lakibd[i, j + 1] == 0 and idomain[0, i, j + 1]:
                        h = [ilak, nlakecon[ilak], (0, i, j + 1), 'horizontal',
                             lak_leakance, 0.0, 0.0, delr / 2., delc]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # front
                if i < nrow - 1:
                    if lakibd[i + 1, j] == 0 and idomain[0, i + 1, j]:
                        h = [ilak, nlakecon[ilak], (0, i + 1, j), 'horizontal',
                             lak_leakance, 0.0, 0.0, delc / 2., delr]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # vertical
                v = [ilak, nlakecon[ilak], (1, i, j), 'vertical', lak_leakance,
                     0.0, 0.0, 0.0, 0.0]
                nlakecon[ilak] += 1
                lakeconnectiondata.append(v)

    i, j = np.where(lakibd > 0)
    idomain[0, i, j] = 0
    gwf.dis.idomain.set_data(idomain[0], layer=0, multiplier=[1])

    lakpackagedata = [[0, 44., nlakecon[0], 'lake1'],
                      [1, 35.2, nlakecon[1], 'lake2']]
    # <outletno> <lakein> <lakeout> <couttype> <invert> <width> <rough> <slope>
    outlets = [[0, 0, -1, 'MANNING', 44.5, 5.000000, 0.03, 0.2187500E-02]]

    lake_on = True
    if lake_on:
        lak = flopy.mf6.ModflowGwflak(gwf, time_conversion=86400.000,
                                      print_stage=True, print_flows=True,
                                      stage_filerecord=gwfname + '.lak.bin',
                                      budget_filerecord=gwfname + '.lak.bud',
                                      mover=True, pname='LAK-1',
                                      boundnames=True,
                                      nlakes=2, noutlets=len(outlets),
                                      outlets=outlets,
                                      packagedata=lakpackagedata,
                                      connectiondata=lakeconnectiondata)

    mover_on = True
    if mover_on:
        maxmvr, maxpackages = 2, 2
        mvrpack = [['SFR-1'], ['LAK-1']]
        mvrperioddata = [
            ['SFR-1', 5, 'LAK-1', 0, 'FACTOR', 1.],
            ['LAK-1', 0, 'SFR-1', 6, 'FACTOR', 1.],
        ]
        mvr = flopy.mf6.ModflowGwfmvr(gwf, maxmvr=maxmvr,
                                      print_flows=True,
                                      maxpackages=maxpackages,
                                      packages=mvrpack,
                                      perioddata=mvrperioddata)

    transport = True
    if transport:

        gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname)

        # ims
        hclose = 0.001
        rclose = 0.001
        nouter = 50
        ninner = 20
        relax = 0.97
        imsgwt = flopy.mf6.ModflowIms(sim, print_option='ALL',
                                      outer_hclose=hclose,
                                      outer_maximum=nouter,
                                      under_relaxation='NONE',
                                      inner_maximum=ninner,
                                      inner_hclose=hclose,
                                      rcloserecord=rclose,
                                      linear_acceleration='BICGSTAB',
                                      scaling_method='NONE',
                                      reordering_method='NONE',
                                      relaxation_factor=relax,
                                      filename='{}.ims'.format(gwtname))
        sim.register_ims_package(imsgwt, [gwt.name])

        dis = flopy.mf6.ModflowGwtdis(gwt, nlay=nlay, nrow=nrow, ncol=ncol,
                                      delr=delr, delc=delc,
                                      top=top, botm=botm, idomain=idomain)
        ic = flopy.mf6.ModflowGwtic(gwt, strt=0.)
        mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.3)
        adv = flopy.mf6.ModflowGwtadv(gwt, scheme='TVD')
        dsp = flopy.mf6.ModflowGwtdsp(gwt, xt3d=True, alh=20., ath1=2, atv=0.2)
        sourcerecarray = [()]
        ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)
        cnclist = [
            [(0, 0, 11), 500.], [(0, 0, 12), 500.], [(0, 0, 13), 500.],
            [(0, 0, 14), 500.],
            [(1, 0, 11), 500.], [(1, 0, 12), 500.], [(1, 0, 13), 500.],
            [(1, 0, 14), 500.],
        ]
        cnc = flopy.mf6.ModflowGwtcnc(gwt, maxbound=len(cnclist),
                                      stress_period_data=cnclist,
                                      save_flows=False,
                                      pname='CNC-1')

        lktpackagedata = [(0, 0., 99., 999., 'mylake1'),
                          (1, 0., 99., 999., 'mylake2'), ]
        lktperioddata = [(0, 'STATUS', 'ACTIVE'),
                         (1, 'STATUS', 'ACTIVE'),
                         ]
        lkt_obs = {(gwtname + '.lkt.obs.csv',):
            [
                ('lkt1conc', 'CONCENTRATION', 1),
                ('lkt2conc', 'CONCENTRATION', 2),
            ],
        }
        lkt_obs['digits'] = 7
        lkt_obs['print_input'] = True
        lkt_obs['filename'] = gwtname + '.lkt.obs'

        lkt_on = True
        if lkt_on:
            lkt = flopy.mf6.modflow.ModflowGwtlkt(gwt,
                                                  boundnames=True,
                                                  save_flows=True,
                                                  print_input=True,
                                                  print_flows=True,
                                                  print_concentration=True,
                                                  concentration_filerecord=gwtname + '.lkt.bin',
                                                  budget_filerecord='.lkt.bud',
                                                  packagedata=lktpackagedata,
                                                  lakeperioddata=lktperioddata,
                                                  observations=lkt_obs,
                                                  pname='LAK-1',
                                                  auxiliary=['aux1', 'aux2'])

        sftpackagedata = []
        for irno in range(sfrpd.shape[0]):
            t = (irno, 0., 99., 999., 'myreach{}'.format(irno + 1))
            sftpackagedata.append(t)

        sftperioddata = [
            (0, 'STATUS', 'ACTIVE'),
            (0, 'CONCENTRATION', 0.)
        ]

        sft_obs = {(gwtname + '.sft.obs.csv',):
                       [('sft{}conc'.format(i + 1), 'CONCENTRATION', i + 1) for
                        i in range(sfrpd.shape[0])]}
        # append additional obs attributes to obs dictionary
        sft_obs['digits'] = 7
        sft_obs['print_input'] = True
        sft_obs['filename'] = gwtname + '.sft.obs'

        sft_on = True
        if sft_on:
            sft = flopy.mf6.modflow.ModflowGwtsft(gwt,
                                                  boundnames=True,
                                                  save_flows=True,
                                                  print_input=True,
                                                  print_flows=True,
                                                  print_concentration=True,
                                                  concentration_filerecord=gwtname + '.sft.bin',
                                                  budget_filerecord=gwtname +
                                                                    '.sft.bud',
                                                  packagedata=sftpackagedata,
                                                  reachperioddata=sftperioddata,
                                                  observations=sft_obs,
                                                  pname='SFR-1',
                                                  auxiliary=['aux1', 'aux2'])

        oc = flopy.mf6.ModflowGwtoc(gwt,
                                    budget_filerecord='{}.cbc'.format(gwtname),
                                    concentration_filerecord='{}.ucn'.format(
                                        gwtname),
                                    concentrationprintrecord=[
                                        ('COLUMNS', ncol, 'WIDTH', 15,
                                         'DIGITS', 6, 'GENERAL')],
                                    saverecord=[('CONCENTRATION', 'ALL'),
                                                ('BUDGET', 'ALL')],
                                    printrecord=[('CONCENTRATION', 'ALL'),
                                                 ('BUDGET', 'ALL')])

        # GWF GWT exchange
        gwfgwt = flopy.mf6.ModflowGwfgwt(sim, exgtype='GWF6-GWT6',
                                         exgmnamea=gwfname, exgmnameb=gwtname)

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def make_plot(sim):
    print('making plots...')
    name = ex[sim.idxsim]
    ws = exdirs[sim.idxsim]
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws)
    gwfname = 'gwf_' + name
    gwtname = 'gwt_' + name
    gwf = sim.get_model(gwfname)
    gwt = sim.get_model(gwtname)

    fname = gwtname + '.lkt.bin'
    fname = os.path.join(ws, fname)
    bobj = flopy.utils.HeadFile(fname, precision='double', text='concentration')
    lkaconc = bobj.get_alldata()[:, 0, 0, :]
    times = bobj.times
    bobj.file.close()

    fname = gwtname + '.sft.bin'
    fname = os.path.join(ws, fname)
    bobj = flopy.utils.HeadFile(fname, precision='double', text='concentration')
    sfaconc = bobj.get_alldata()[:, 0, 0, :]
    times = bobj.times
    bobj.file.close()

    import matplotlib.pyplot as plt
    plt.figure(figsize=(8, 5))
    times = np.array(times) / 365.
    plt.plot(times, lkaconc[:, 0], 'b-', label='Lake 1')
    plt.plot(times, sfaconc[:, 30], 'r-', label='Stream segment 3')
    plt.plot(times, sfaconc[:, 37], 'g-', label='Stream segment 4')
    plt.legend()
    plt.ylim(0, 50)
    plt.xlim(0, 25)
    plt.xlabel('TIME, IN YEARS')
    plt.ylabel('SIMULATED BORON CONCENTRATION,\nIN MICROGRAMS PER LITER')
    plt.draw()
    fname = os.path.join(ws, name + '.png')
    plt.savefig(fname)

    return


def eval_results(sim):
    print('evaluating results...')

    makeplot = False
    if makeplot:
        make_plot(sim)

    # ensure concentrations were saved
    ws = exdirs[sim.idxsim]
    name = ex[sim.idxsim]
    gwtname = 'gwt_' + name

    fname = gwtname + '.lkt.bin'
    fname = os.path.join(ws, fname)
    bobj = flopy.utils.HeadFile(fname, precision='double', text='concentration')
    lkaconc = bobj.get_alldata()[:, 0, 0, :]
    times = bobj.times
    bobj.file.close()

    fname = gwtname + '.sft.bin'
    fname = os.path.join(ws, fname)
    bobj = flopy.utils.HeadFile(fname, precision='double', text='concentration')
    sfaconc = bobj.get_alldata()[:, 0, 0, :]
    times = bobj.times
    bobj.file.close()

    # check simulated concentration in lak 1 and 2 sfr reaches
    res_lak1 = lkaconc[:, 0]
    ans_lak1 = \
        [-5.19192651e-19,  6.35166416e-02,  4.38454911e-01,  1.54130198e+00,
          3.72664560e+00,  7.06084145e+00,  1.12708627e+01,  1.58912818e+01,
          2.04612191e+01,  2.46405192e+01,  2.82440035e+01,  3.12265813e+01,
          3.35908554e+01,  3.54073231e+01,  3.67722923e+01,  3.77806563e+01,
          3.85227989e+01,  3.90670861e+01,  3.94641768e+01,  3.97535991e+01,
          3.99669840e+01,  4.01262471e+01,  4.02470985e+01,  4.03391824e+01,
          4.04108589e+01,  4.04673231e+01]
    assert np.allclose(res_lak1, ans_lak1), '{} {}'.format(res_lak1, ans_lak1)

    res_sfr3 = sfaconc[:, 30]
    ans_sfr3 = \
        [-1.44897285e-20,  5.37716726e-03,  3.90779641e-02,  1.46239972e-01,
          3.80931689e-01,  7.88290773e-01,  1.39471354e+00,  2.21138848e+00,
          3.24097611e+00,  4.48247263e+00,  5.92772241e+00,  7.55687364e+00,
          9.34017271e+00,  1.12552713e+01,  1.32516321e+01,  1.52890518e+01,
          1.73388875e+01,  1.93957919e+01,  2.14081754e+01,  2.33360511e+01,
          2.51463391e+01,  2.68341700e+01,  2.83711539e+01,  2.97397682e+01,
          3.09364802e+01,  3.19695161e+01]
    assert np.allclose(res_sfr3, ans_sfr3), '{} {}'.format(res_sfr3, ans_sfr3)

    res_sfr4 = sfaconc[:, 37]
    ans_sfr4 = \
        [-3.81844339e-18,  3.77280297e-02,  2.61266478e-01,  9.22679324e-01,
          2.24293981e+00,  4.27944410e+00,  6.89006955e+00,  9.82346043e+00,
          1.28233817e+01,  1.57007518e+01,  1.83473308e+01,  2.07288599e+01,
          2.28354658e+01,  2.46962752e+01,  2.63416514e+01,  2.78068135e+01,
          2.91280420e+01,  3.03423574e+01,  3.14564411e+01,  3.24729884e+01,
          3.33949019e+01,  3.42319415e+01,  3.49798247e+01,  3.56380299e+01,
          3.62092533e+01,  3.66991479e+01]
    assert np.allclose(res_sfr4, ans_sfr4), '{} {}'.format(res_sfr4, ans_sfr4)

    # uncomment when testing
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
