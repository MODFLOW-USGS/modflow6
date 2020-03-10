"""
MODFLOW 6 Autotest
Test to make sure that recharge is passed to the highest active layer and
verify that recharge is in the highest active layer by looking at the
individual budget terms.  For this test, there are two layers and five
columns.  The top layer is dry except for the middle cell.  Recharge is
applied to the top layer.  In the test a, IRCH is not specified.  In test b
IRCH is specified as 1, and in test c IRCH is specified as [2, 2, 1, 2, 2]
"""

import os
import sys
import numpy as np
from ctypes import *

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

ex = ['libgwf_rch02']
exdirs = []
for s in ex:
    exdirs.append(os.path.join('temp', s))

# average recharge rate
avg_rch = 0.001

# calculate recharge rates
dx = 1/20
rad = np.arange(0, 1 + dx, dx) * 2. * np.pi
f = np.sin(rad)
rch_rates = avg_rch + f * avg_rch

# temporal discretization
nper = rch_rates.shape[0]
tdis_rc = []
for i in range(nper):
    tdis_rc.append((1., 1, 1))

# model spatial dimensions
nlay, nrow, ncol = 1, 1, 100

# cell spacing
delr = 50.
delc = 1.

# top of the aquifer
top = 25.

# bottom of the aquifer
botm = 0.

# hydraulic conductivity
hk = 50.

# boundary heads
h1 = 20.
h2 = 11.

# build chd stress period data
chd_spd = {0: [[(0, 0, 0), h1],
               [(0, 0, ncol-1), h2]]}

# build recharge spd
rch_spd = {}
for n in range(nper):
    rch_spd[n] = rch_rates[n]

# solver data
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97

def build_model(ws, name, rech):
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name='mf6',
                                 sim_ws=ws, memory_print_option='all')
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               outer_hclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='DBD',
                               inner_maximum=ninner,
                               inner_hclose=hclose, rcloserecord=rclose,
                               linear_acceleration='BICGSTAB',
                               relaxation_factor=relax)

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)

    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=top)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True,
                                  icelltype=1,
                                  k=hk)

    # chd file
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)

    # recharge file
    rch = flopy.mf6.ModflowGwfrcha(gwf, recharge=rech)

    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL')],
                                printrecord=[('HEAD', 'ALL'),
                                             ('BUDGET', 'ALL')])
    return sim

def get_model(idx, dir):

    # build MODFLOW 6 files
    ws = dir
    name = ex[idx]
    sim = build_model(ws, name, rech=rch_spd)

    # build comparison model
    ws = os.path.join(dir, 'libmf6')
    # mc = build_model(ws, name, rech=avg_rch)
    mc = build_model(ws, name, rech=rch_spd)

    return sim, mc


def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_simulation()
    return

def bmifunc(exe, idx, model_ws=None):

    success = False

    name = ex[idx].upper()
    init_wd = os.path.abspath(os.getcwd())
    if model_ws is not None:
        os.chdir(model_ws)

    mf6 = cdll.LoadLibrary(exe)

    # initialize the model
    statcode = mf6.bmi_initialize()

    # check for error condition
    if statcode != 0:
        return success

    # get times
    ct = c_double(0.0)
    mf6.get_current_time(byref(ct))
    et = c_double(0.0)
    mf6.get_end_time(byref(et))

    # get variable size(s)
    elsize = c_int(0)
    nbytes = c_int(0)
    rchname = c_char_p(b"LIBGWF_RCH02 RCHA/RHS")
    mf6.get_var_itemsize(rchname, byref(elsize))
    mf6.get_var_nbytes(rchname, byref(nbytes))
    nsize = int(nbytes.value / elsize.value)

    # set the function prototype and declare the receiving pointers-to-array
    arraytype = np.ctypeslib.ndpointer(dtype="double", ndim=1, shape=(nsize,),
                                       flags='F')
    mf6.get_value_ptr_double.argtypes = [c_char_p, POINTER(arraytype)]
    mf6.get_value_ptr_double.restype = c_int
    mf6.get_value_double.argtypes = [c_char_p, arraytype, POINTER(c_int)]
    mf6.get_value_double.restype = c_int

    # rch = arraytype()
    #
    # localArray = np.zeros(nsize, dtype="double", order='F')

    # model time loop
    while ct.value < et.value:
        # calculate
        statcode = mf6.bmi_update()

        # check for error condition
        if statcode != 0:
            return success

        # update time
        mf6.get_current_time(byref(ct))

    # cleanup
    statcode = mf6.bmi_finalize()

    if model_ws is not None:
        os.chdir(init_wd)

    # cleanup and return
    return True

# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, idxsim=idx, bmifunc=bmifunc)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, idxsim=idx, bmifunc=bmifunc)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
