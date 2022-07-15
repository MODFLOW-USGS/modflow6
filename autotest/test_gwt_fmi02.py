# tests to ability to run flow model first followed by transport model

import os
import shutil

import numpy as np
import pytest

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)


import targets

exe_name_mf6 = targets.target_dict["mf6"]
exe_name_mf6 = os.path.abspath(exe_name_mf6)
testdir = "./temp"
testgroup = "fmi02"
d = os.path.join(testdir, testgroup)
if os.path.isdir(d):
    shutil.rmtree(d)


def run_flow_model():
    name = "flow"
    ws = os.path.join(testdir, testgroup, name)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=ws, exe_name=exe_name_mf6
    )
    pd = [(1.0, 1, 1.0), (1.0, 1, 1.0)]
    tdis = flopy.mf6.ModflowTdis(sim, nper=len(pd), perioddata=pd)
    ims = flopy.mf6.ModflowIms(sim)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(gwf, nrow=10, ncol=10)
    ic = flopy.mf6.ModflowGwfic(gwf)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_specific_discharge=True, save_saturation=True
    )
    spd = {
        0: [[(0, 0, 0), 1.0, 1.0], [(0, 9, 9), 0.0, 0.0]],
        1: [[(0, 0, 0), 0.0, 0.0], [(0, 9, 9), 1.0, 2.0]],
    }
    chd = flopy.mf6.ModflowGwfchd(
        gwf, pname="CHD-1", stress_period_data=spd, auxiliary=["concentration"]
    )
    budget_file = name + ".bud"
    head_file = name + ".hds"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=budget_file,
        head_filerecord=head_file,
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )
    sim.write_simulation()
    sim.run_simulation()
    fname = os.path.join(ws, budget_file)
    assert os.path.isfile(fname)
    fname = os.path.join(ws, head_file)
    assert os.path.isfile(fname)
    return


def run_transport_model():
    name = "transport"
    ws = os.path.join(testdir, testgroup, name)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=ws, exe_name=exe_name_mf6
    )
    pd = [(1.0, 10, 1.0), (1.0, 10, 1.0)]
    tdis = flopy.mf6.ModflowTdis(sim, nper=len(pd), perioddata=pd)
    ims = flopy.mf6.ModflowIms(sim, linear_acceleration="BICGSTAB")
    gwt = flopy.mf6.ModflowGwt(sim, modelname=name, save_flows=True)
    dis = flopy.mf6.ModflowGwtdis(gwt, nrow=10, ncol=10)
    ic = flopy.mf6.ModflowGwtic(gwt)
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.2)
    adv = flopy.mf6.ModflowGwtadv(gwt)
    pd = [("GWFBUDGET", "../flow/flow.bud", None)]
    fmi = flopy.mf6.ModflowGwtfmi(gwt, packagedata=pd)
    sources = [("CHD-1", "AUX", "CONCENTRATION")]
    ssm = flopy.mf6.ModflowGwtssm(gwt, print_flows=True, sources=sources)
    budget_file = name + ".bud"
    concentration_file = name + ".ucn"
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=budget_file,
        concentration_filerecord=concentration_file,
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )
    sim.write_simulation()
    sim.run_simulation()
    fname = os.path.join(ws, budget_file)
    assert os.path.isfile(fname)
    fname = os.path.join(ws, concentration_file)
    assert os.path.isfile(fname)
    return


def test_fmi():
    run_flow_model()
    run_transport_model()
    d = os.path.join(testdir, testgroup)
    if os.path.isdir(d):
        shutil.rmtree(d)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run tests
    test_fmi()
