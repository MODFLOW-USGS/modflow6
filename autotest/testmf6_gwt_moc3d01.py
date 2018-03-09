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

ex = ['moc3d01a', 'moc3d01b', 'moc3d01c', 'moc3d01d']
diffc = [0., 0.01, 0., 0.1]
alphal = [0.1, 0., 1., 0.]
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))
ddir = 'data'


def build_models():
    nlay, nrow, ncol = 1, 122, 1
    nper = 1
    perlen = [120.]
    nstp = [240]
    tsmult = [1.]
    steady = [True]
    delr = 0.1
    delc = 0.1
    top = 1.
    botm = [0.]
    strt = 1.
    hnoflo = 1e30
    hdry = -1e30
    hk = 0.01
    laytyp = 0
    #ss = 0.
    #sy = 0.1

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-8, 1e-6, 1.

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
        # storage
        #sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False,
        #                              iconvert=laytyp[idx],
        #                              ss=ss[idx], sy=sy[idx],
        #                              steady_state={0: True, 2: True},
        #                              transient={1: True})

        # chd files
        c = {0: [[(0, 121, 0), 0.0000000]]}
        chd = flopy.mf6.ModflowGwfchd(gwf,
                                      stress_period_data=c,
                                      save_flows=False,
                                      pname='CHD-1')

        # wel files
        w = {0: [[(0, 0, 0), 0.001, 1.0]]}
        wel = flopy.mf6.ModflowGwfwel(gwf,
                                      print_input=True,
                                      print_flows=True,
                                      stress_period_data=w,
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
        adv = flopy.mf6.ModflowGwtadv(gwt, scheme='UPSTREAM',
                                    fname='{}.adv'.format(gwtname))

        # dispersion
        dsp = flopy.mf6.ModflowGwtdsp(gwt, xt3d=True, diffc=diffc[idx],
                                      alh=alphal[idx], alv=alphal[idx],
                                      ath=0.0, atv=0.0,
                                      fname='{}.dsp'.format(gwtname))

        # constant concentration
        #cncs = {0: [[(0, 0, 0), 1.0]]}
        #cnc = flopy.mf6.ModflowGwtcnc(gwt, maxbound=len(cncs),
        #                              stress_period_data=cncs,
        #                              save_flows=False,
        #                              pname='CNC-1')

        # storage
        sto = flopy.mf6.ModflowGwtsto(gwt, porosity=0.1,
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
        station = [(0, 1, 0), (0, 40, 0), (0, 110, 0)]
        tssim = cobj.get_ts(station)[::5]
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    tsresab = [[5.00000000e-01, 1.23105626e-01, 1.33368154e-15, 1.14620505e-40],
       [3.00000000e+00, 6.30007267e-01, 6.04938182e-11, 5.34140505e-34],
       [5.50000000e+00, 8.28096434e-01, 2.12036964e-08, 1.28425275e-29],
       [8.00000000e+00, 9.13419702e-01, 1.14989181e-06, 3.33889670e-26],
       [1.05000000e+01, 9.54171515e-01, 2.12627604e-05, 2.14865421e-23],
       [1.30000000e+01, 9.74914921e-01, 1.94057699e-04, 5.11639575e-21],
       [1.55000000e+01, 9.85931671e-01, 1.07834303e-03, 5.71799725e-19],
       [1.80000000e+01, 9.91962912e-01, 4.16395774e-03, 3.50733434e-17],
       [2.05000000e+01, 9.95340982e-01, 1.22120565e-02, 1.31852458e-15],
       [2.30000000e+01, 9.97267022e-01, 2.89592073e-02, 3.29625818e-14],
       [2.55000000e+01, 9.98380999e-01, 5.81163481e-02, 5.83351707e-13],
       [2.80000000e+01, 9.99032919e-01, 1.02125083e-01, 7.67697759e-12],
       [3.05000000e+01, 9.99418210e-01, 1.61290150e-01, 7.81609345e-11],
       [3.30000000e+01, 9.99647835e-01, 2.33633253e-01, 6.35908611e-10],
       [3.55000000e+01, 9.99785676e-01, 3.15411511e-01, 4.24699566e-09],
       [3.80000000e+01, 9.99868941e-01, 4.01990998e-01, 2.38156350e-08],
       [4.05000000e+01, 9.99919516e-01, 4.88733882e-01, 1.14304837e-07],
       [4.30000000e+01, 9.99950385e-01, 5.71668510e-01, 4.77334416e-07],
       [4.55000000e+01, 9.99969310e-01, 6.47857184e-01, 1.75911390e-06],
       [4.80000000e+01, 9.99980957e-01, 7.15487009e-01, 5.79201076e-06],
       [5.05000000e+01, 9.99988150e-01, 7.73764532e-01, 1.72229561e-05],
       [5.30000000e+01, 9.99992607e-01, 8.22704655e-01, 4.66919669e-05],
       [5.55000000e+01, 9.99995376e-01, 8.62887974e-01, 1.16376874e-04],
       [5.80000000e+01, 9.99997102e-01, 8.95235357e-01, 2.68661986e-04],
       [6.05000000e+01, 9.99998180e-01, 9.20824808e-01, 5.78275393e-04],
       [6.30000000e+01, 9.99998855e-01, 9.40758103e-01, 1.16740945e-03],
       [6.55000000e+01, 9.99999278e-01, 9.56073862e-01, 2.22218433e-03],
       [6.80000000e+01, 9.99999544e-01, 9.67698569e-01, 4.00760565e-03],
       [7.05000000e+01, 9.99999712e-01, 9.76425599e-01, 6.87729354e-03],
       [7.30000000e+01, 9.99999817e-01, 9.82913088e-01, 1.12741038e-02],
       [7.55000000e+01, 9.99999884e-01, 9.87693178e-01, 1.77185495e-02],
       [7.80000000e+01, 9.99999927e-01, 9.91187125e-01, 2.67836299e-02],
       [8.05000000e+01, 9.99999953e-01, 9.93722511e-01, 3.90569795e-02],
       [8.30000000e+01, 9.99999970e-01, 9.95550228e-01, 5.50936561e-02],
       [8.55000000e+01, 9.99999981e-01, 9.96859912e-01, 7.53648477e-02],
       [8.80000000e+01, 9.99999988e-01, 9.97793261e-01, 1.00208846e-01],
       [9.05000000e+01, 9.99999992e-01, 9.98455090e-01, 1.29790590e-01],
       [9.30000000e+01, 9.99999995e-01, 9.98922235e-01, 1.64074949e-01],
       [9.55000000e+01, 9.99999997e-01, 9.99250576e-01, 2.02816994e-01],
       [9.80000000e+01, 9.99999998e-01, 9.99480462e-01, 2.45570161e-01],
       [1.00500000e+02, 9.99999999e-01, 9.99640840e-01, 2.91710911e-01],
       [1.03000000e+02, 9.99999999e-01, 9.99752357e-01, 3.40476587e-01],
       [1.05500000e+02, 9.99999999e-01, 9.99829663e-01, 3.91011978e-01],
       [1.08000000e+02, 1.00000000e+00, 9.99883101e-01, 4.42419555e-01],
       [1.10500000e+02, 1.00000000e+00, 9.99919944e-01, 4.93808594e-01],
       [1.13000000e+02, 1.00000000e+00, 9.99945283e-01, 5.44339147e-01],
       [1.15500000e+02, 1.00000000e+00, 9.99962670e-01, 5.93257930e-01],
       [1.18000000e+02, 1.00000000e+00, 9.99974576e-01, 6.39924426e-01]]

    tsrescd = [[5.00000000e-01, 1.09104387e-01, 2.90614069e-08, 4.60152213e-20],
       [3.00000000e+00, 3.88089395e-01, 4.29101624e-05, 4.52743132e-15],
       [5.50000000e+00, 5.22013308e-01, 9.28475403e-04, 3.00151766e-12],
       [8.00000000e+00, 6.09675204e-01, 5.36809069e-03, 2.78965324e-10],
       [1.05000000e+01, 6.73406725e-01, 1.65488836e-02, 8.23601461e-09],
       [1.30000000e+01, 7.22350626e-01, 3.60307675e-02, 1.14054425e-07],
       [1.55000000e+01, 7.61248161e-01, 6.35177433e-02, 9.26856412e-07],
       [1.80000000e+01, 7.92901867e-01, 9.76258349e-02, 5.09437119e-06],
       [2.05000000e+01, 8.19113810e-01, 1.36608405e-01, 2.08394702e-05],
       [2.30000000e+01, 8.41113102e-01, 1.78777687e-01, 6.78516397e-05],
       [2.55000000e+01, 8.59775471e-01, 2.22687416e-01, 1.84591798e-04],
       [2.80000000e+01, 8.75746201e-01, 2.67180667e-01, 4.34990051e-04],
       [3.05000000e+01, 8.89513641e-01, 3.11374468e-01, 9.12381481e-04],
       [3.30000000e+01, 9.01455481e-01, 3.54620486e-01, 1.73932636e-03],
       [3.55000000e+01, 9.11869128e-01, 3.96460852e-01, 3.06315296e-03],
       [3.80000000e+01, 9.20992343e-01, 4.36587295e-01, 5.04807947e-03],
       [4.05000000e+01, 9.29017672e-01, 4.74806411e-01, 7.86532846e-03],
       [4.30000000e+01, 9.36102788e-01, 5.11011454e-01, 1.16827365e-02],
       [4.55000000e+01, 9.42378065e-01, 5.45160007e-01, 1.66551246e-02],
       [4.80000000e+01, 9.47952227e-01, 5.77256621e-01, 2.29163068e-02],
       [5.05000000e+01, 9.52916626e-01, 6.07339463e-01, 3.05732046e-02],
       [5.30000000e+01, 9.57348536e-01, 6.35470137e-01, 3.97021909e-02],
       [5.55000000e+01, 9.61313715e-01, 6.61725988e-01, 5.03475327e-02],
       [5.80000000e+01, 9.64868427e-01, 6.86194324e-01, 6.25216460e-02],
       [6.05000000e+01, 9.68061046e-01, 7.08968103e-01, 7.62067964e-02],
       [6.30000000e+01, 9.70933350e-01, 7.30142756e-01, 9.13578647e-02],
       [6.55000000e+01, 9.73521567e-01, 7.49813869e-01, 1.07905817e-01],
       [6.80000000e+01, 9.75857229e-01, 7.68075518e-01, 1.25761566e-01],
       [7.05000000e+01, 9.77967876e-01, 7.85019098e-01, 1.44819961e-01],
       [7.30000000e+01, 9.79877637e-01, 8.00732536e-01, 1.64963709e-01],
       [7.55000000e+01, 9.81607711e-01, 8.15299771e-01, 1.86067081e-01],
       [7.80000000e+01, 9.83176779e-01, 8.28800464e-01, 2.07999300e-01],
       [8.05000000e+01, 9.84601341e-01, 8.41309844e-01, 2.30627546e-01],
       [8.30000000e+01, 9.85896004e-01, 8.52898686e-01, 2.53819566e-01],
       [8.55000000e+01, 9.87073731e-01, 8.63633362e-01, 2.77445857e-01],
       [8.80000000e+01, 9.88146047e-01, 8.73575951e-01, 3.01381466e-01],
       [9.05000000e+01, 9.89123218e-01, 8.82784401e-01, 3.25507403e-01],
       [9.30000000e+01, 9.90014406e-01, 8.91312699e-01, 3.49711718e-01],
       [9.55000000e+01, 9.90827804e-01, 8.99211074e-01, 3.73890276e-01],
       [9.80000000e+01, 9.91570744e-01, 9.06526202e-01, 3.97947254e-01],
       [1.00500000e+02, 9.92249805e-01, 9.13301408e-01, 4.21795426e-01],
       [1.03000000e+02, 9.92870892e-01, 9.19576881e-01, 4.45356240e-01],
       [1.05500000e+02, 9.93439317e-01, 9.25389867e-01, 4.68559745e-01],
       [1.08000000e+02, 9.93959862e-01, 9.30774870e-01, 4.91344389e-01],
       [1.10500000e+02, 9.94436839e-01, 9.35763837e-01, 5.13656716e-01],
       [1.13000000e+02, 9.94874139e-01, 9.40386336e-01, 5.35450987e-01],
       [1.15500000e+02, 9.95275277e-01, 9.44669726e-01, 5.56688746e-01],
       [1.18000000e+02, 9.95643434e-01, 9.48639312e-01, 5.77338348e-01]]
    tsresab = np.array(tsresab)
    tsrescd = np.array(tsrescd)

    tsreslist = [tsresab, tsresab, tsrescd, tsrescd]
    tsres = tsreslist[sim.idxsim]
    assert np.allclose(tsres, tssim), 'simulated concentrations do not match with known solution.'

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
