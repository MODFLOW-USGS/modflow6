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

ex = ['lnf_chd_wel']
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
perlen, nstp, tsmult = 1000.000, 10, 1.
tdis_rc = [[perlen, nstp, tsmult]]

# solver parameters
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.

# starting heads
strt = [200., 200., 200.]

# chd cells
chd_dict = {0: [((2,), 200.0)]}

# wel cells
wel_dict = {0: [((0,), -100.0)]}

def get_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws)
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='SECONDS',
                                 nper=nper, perioddata=tdis_rc)

    # create iterative model solution
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               outer_hclose=hclose,
                               outer_maximum=nouter,
                               inner_maximum=ninner,
                               inner_hclose=hclose, rcloserecord=rclose,
                               linear_acceleration='CG',
                               relaxation_factor=relax)

    # create gwf model
    lnf = flopy.mf6.ModflowLnf(sim, modelname=name, save_flows=True)

    dis = flopy.mf6.ModflowLnfdisl(lnf, length_units='METERS',
                                   time_conversion=1.0,
                                   length_conversion=1.0,
                                   nodes=nodes, nvert=nvert,
                                   idomain=1,
                                   vertices=vertices,
                                   cell1d=cell1d)

    # create geometry packages # for scott to fix
    cgeo = flopy.mf6.ModflowLnfcgeo(lnf, print_input=True,
                                    ngeo=len(gdc), geometry_data=gdc)

    # initial conditions
    ic = flopy.mf6.ModflowLnfic(lnf, strt=strt)

    # flow and storage
    npf = flopy.mf6.ModflowLnfnpfl(lnf, print_flows=True, viscosity=0.001002)#26.33472)
    sto = flopy.mf6.ModflowLnfsto(lnf, save_flows=True, iconvert=0,
                                  transient={0: True})

    # chd file
    chd = flopy.mf6.ModflowLnfchd(lnf, print_input=True, print_flows=True,
                                  stress_period_data=chd_dict)

    # wel file
    wel = flopy.mf6.ModflowLnfwel(lnf, print_input=True, print_flows=True,
                                  stress_period_data=wel_dict)

    # output control
    oc = flopy.mf6.ModflowLnfoc(lnf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'ALL'),
                                             ('BUDGET', 'ALL')])

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
