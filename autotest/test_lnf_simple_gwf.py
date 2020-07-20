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

ex = ['lnf_smp_gwf']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))

## run all examples on Travis
travis = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
# spatial discretization
nodes, nvert = 3, 4

# all cells are active
ib = 1

# vertices and cell1d
vertices = [(0, 0., 5000., 100.),
            (1, 0., 4000., 100.),
            (2, 0., 3000., 100.),
            (3, 0., 2000., 100.)]

cell1d = [(0, 0.5, 2, 0, 1),
          (1, 0.5, 2, 1, 2),
          (2, 0.5, 2, 2, 3)]

# geometry data
gdc = [((0,), 0.1),
       ((1,), 0.1),
       ((2,), 0.1)]

# temporal discretization
nper = 1
perlen, nstp, tsmult = 6., 2, 1.
tdis_rc = [[perlen, nstp, tsmult]]

# solver parameters
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-6, 1e-5, 1.

# starting heads
strt = 101.


# SUB package problem 3
def get_model(idx, dir):
    name = ex[idx]
    lnfname = '{}_lnf'.format(name)
    gwfname = '{}_gwf'.format(name)

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    # create iterative model solution
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               outer_hclose=hclose,
                               outer_maximum=nouter,
                               inner_maximum=ninner,
                               inner_hclose=hclose, rcloserecord=rclose,
                               linear_acceleration='CG',
                               relaxation_factor=relax)

    # create lnf model
    lnf = flopy.mf6.ModflowLnf(sim, modelname=lnfname, save_flows=True)

    disl = flopy.mf6.ModflowLnfdisl(lnf, length_units='METERS',
                                   time_conversion=86400.0,
                                   length_conversion=3.28081,
                                   nodes=nodes, nvert=nvert,
                                   idomain=1,
                                   vertices=vertices,
                                   cell1d=cell1d)

    # create geometry packages # for scott to fix
    cgeo = flopy.mf6.ModflowLnfcgeo(lnf, print_input=True,
                                    ngeo=len(gdc), geometry_data=gdc)

    # initial conditions
    ic = flopy.mf6.ModflowLnfic(lnf, strt=101.0)

    # flow and storage
    npf = flopy.mf6.ModflowLnfnpfl(lnf, print_flows=True, viscosity=1.0e-6)
    sto = flopy.mf6.ModflowLnfsto(lnf, save_flows=True, iconvert=0,
                                  transient={0: True})

    # output control
    oc = flopy.mf6.ModflowLnfoc(lnf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'LAST'),
                                             ('BUDGET', 'ALL')])

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname,
                               model_nam_file='{}.nam'.format(gwfname))
    dis_package = flopy.mf6.ModflowGwfdis(
        gwf, length_units='FEET', nlay=1, nrow=1, ncol=10, delr=500.0,
        delc=500.0, top=100.0, botm=50.0, filename='{}.dis'.format(gwfname),
        pname='mydispkg')
    strt = [100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
            100.0]
    ic_package = flopy.mf6.ModflowGwfic(gwf, strt=strt,
                                        filename='{}.ic'.format(gwfname))
    npf_package = flopy.mf6.ModflowGwfnpf(
        gwf, pname='npf_1', save_flows=True,
        alternative_cell_averaging='logarithmic', icelltype=1, k=5.0)
    oc_package = flopy.mf6.ModflowGwfoc(
        gwf, budget_filerecord=[('np001_mod.cbc',)],
        head_filerecord=[('np001_mod.hds',)],
        saverecord={0: [('HEAD', 'ALL'), ('BUDGET', 'ALL')]},
        printrecord={0: [('HEAD', 'ALL'), ('BUDGET', 'ALL')]})
    sto_package = flopy.mf6.ModflowGwfsto(gwf, save_flows=True, iconvert=1,
                                          ss=0.000001, sy=0.15)
    well_spd = {0: [((0, 0, 4), -2000.0), ((0, 0, 7), -2.0)]}
    wel_package = flopy.mf6.ModflowGwfwel(
        gwf, print_input=True, print_flows=True, save_flows=True, maxbound=2,
        stress_period_data=well_spd)
    drn_package = flopy.mf6.ModflowGwfdrn(
        gwf, print_input=True, print_flows=True, save_flows=True, maxbound=1,
        stress_period_data=[((0, 0, 0), 80, 60.0)])
    riv_spd = {0: [((0, 0, 9), 110, 90.0, 100.0, 1.0, 2.0, 3.0)]}
    riv_package = flopy.mf6.ModflowGwfriv(
        gwf, print_input=True, print_flows=True, save_flows=True, maxbound=1,
        auxiliary=['var1', 'var2', 'var3'], stress_period_data=riv_spd)

    # build exchange
    exchange_data = [((0,0,0), (2,), 1, 0, 0, 0, 5.0, 10.0, 10.0),
                     ((0,0,9), (0,), 1, 0, 0, 0, 5.0, 10.0, 10.0)]
    exg_package = flopy.mf6.ModflowGwflnf(sim, print_input=True, print_flows=True,
                                save_flows=True,
                                nexg=2, exchangedata=exchange_data,
                                exgtype='gwf6-lnf6', exgmnamea=gwfname,
                                exgmnameb=lnfname)

    return sim, None


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_input()
    return


def test_mf6model():
    # determine if running on Travis
    is_travis = 'TRAVIS' in os.environ

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    # for idx, dir in enumerate(exdirs):
    #     if is_travis and not travis[idx]:
    #         continue
    #     yield test.run_mf6, Simulation(dir, exfunc=None,
    #                                    exe_dict=None,
    #                                    idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # # run the test models
    # for idx, dir in enumerate(exdirs):
    #     sim = Simulation(dir, exfunc=None,
    #                      exe_dict=None, idxsim=idx)
    #     test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
