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

ex = ['lnf_disl01']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))

## run all examples on Travis
travis = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
# spatial discretization
nodes, nvert = 8, 29

# all cells are active
ib = 1

# vertices and cell1d
vertices = [(0, 0., 6000., 100.),
            (1, 1000., 5000., 99.),
            (2, 2000., 4000., 98.),
            (3, 2000., 3000., 98.),
            (4, 2000., 3000., 98.),
            (5, 3000., 1000., 97.),
            (6, 4000., 1000., 96.),
            (7, 5000., 1000., 95.),
            (8, 6000., 1000., 94.),
            (9, 7000., 1000., 93.),
            (10, 8000., 2000., 92.),
            (11, 3000., 4000., 97.),
            (12, 4000., 4000., 97.),
            (13, 5000., 4000., 95.),
            (14, 6000., 5000., 94.),
            (15, 7000., 5000., 93.),
            (16, 8000., 5000., 92.),
            (17, 9000., 5000., 91.),
            (18, 10000., 5000., 90.),
            (19, 11000., 5000., 89.),
            (20, 12000., 5000., 88.),
            (21, 6000., 3000., 94.),
            (22, 7000., 2000., 93.),
            (23, 9000., 2000., 91.),
            (24, 10000., 2000., 90.),
            (25, 11000., 2000., 89.),
            (26, 12000., 2000., 88.),
            (27, 13000., 5000., 87.),
            (28, 13000., 2000., 87.)]

cell1d = [(0, 0.5, 3, 0, 1, 2),
          (1, 0.5, 9, 2, 3, 4, 5, 6, 7, 8, 9, 10),
          (2, 0.5, 4, 2, 11, 12, 13),
          (3, 0.5, 8, 13, 14, 15, 16, 17, 18, 19, 20),
          (4, 0.5, 4, 13, 21, 22, 10),
          (5, 0.5, 5, 26, 25, 24, 23, 10),
          (6, 1.0, 2, 20, 27),
          (7, 0.0, 2, 28, 26)]

# temporal discretization
nper = 1
perlen, nstp, tsmult = 1., 1, 1.
tdis_rc = (perlen, nstp, tsmult)

# solver parameters
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.

# starting heads
strt = 101.

# chd data
chd_dict = {0: [((6,), 88., 'coastal'),
                ((7,), 88.25, 'coastal')]}


# SUB package problem 3
def get_model(idx, dir):
    name = ex[idx]

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

    # create gwf model
    lnf = flopy.mf6.ModflowLnf(sim, modelname=name, save_flows=True)

    dis = flopy.mf6.ModflowLnfdisl(lnf, length_units='METERS',
                                   nodes=nodes, nvert=nvert,
                                   # idomain=1,
                                   vertices=vertices,
                                   cell1d=cell1d)

    # initial conditions
    ic = flopy.mf6.ModflowLnfic(lnf, strt=strt)

    # # chd files
    # chd = flopy.mf6.ModflowLnfchd(lnf, stress_period_data=chd_dict,
    #                               boundnames=True)

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
