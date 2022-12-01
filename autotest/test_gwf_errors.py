"""
MODFLOW 6 Autotest
Test to make sure that mf6 is failing with the correct error messages.  This
test script is set up to be extensible so that simple models can be created
very easily and tested with different options to succeed or fail correctly.

"""

import os
import shutil
import subprocess

import numpy as np
import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

import targets
from framework import set_teardown_test

mf6_exe = os.path.abspath(targets.target_dict["mf6"])
testname = "gwf_errors"
testdir = os.path.join("temp", testname)
os.makedirs(testdir, exist_ok=True)
everything_was_successful = True

teardown_test = set_teardown_test()


def run_mf6(argv, ws):
    buff = []
    proc = subprocess.Popen(
        argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=ws
    )
    result, error = proc.communicate()
    if result is not None:
        c = result.decode("utf-8")
        c = c.rstrip("\r\n")
        print(f"{c}")
        buff.append(c)

    return proc.returncode, buff


def run_mf6_error(ws, err_str_list):
    returncode, buff = run_mf6([mf6_exe], ws)
    msg = "mf terminated with error"
    if teardown_test:
        shutil.rmtree(ws, ignore_errors=True)
    if returncode != 0:
        if not isinstance(err_str_list, list):
            err_str_list = list(err_str_list)
        for err_str in err_str_list:
            err = any(err_str in s for s in buff)
            if err:
                raise RuntimeError(msg)
            else:
                msg += " but did not print correct error message."
                msg += f'  Correct message should have been "{err_str}"'
                raise ValueError(msg)


def get_minimal_gwf_simulation(
    ws,
    name="test",
    simkwargs=None,
    simnamefilekwargs=None,
    tdiskwargs=None,
    gwfkwargs=None,
    imskwargs=None,
    diskwargs=None,
    disukwargs=None,
    ickwargs=None,
    npfkwargs=None,
    chdkwargs=None,
):
    if simkwargs is None:
        simkwargs = {}
    if tdiskwargs is None:
        tdiskwargs = {}
    if gwfkwargs is None:
        gwfkwargs = {}
        gwfkwargs["modelname"] = name
    if imskwargs is None:
        imskwargs = {
            "print_option": "SUMMARY",
        }
    if diskwargs is None and disukwargs is None:
        diskwargs = {}
        diskwargs["nlay"] = 5
        diskwargs["nrow"] = 5
        diskwargs["ncol"] = 5
        diskwargs["top"] = 0
        diskwargs["botm"] = [-1, -2, -3, -4, -5]
    if ickwargs is None:
        ickwargs = {}
    if npfkwargs is None:
        npfkwargs = {}
    if chdkwargs is None:
        chdkwargs = {}
        nl = diskwargs["nlay"]
        nr = diskwargs["nrow"]
        nc = diskwargs["ncol"]
        chdkwargs["stress_period_data"] = {
            0: [[(0, 0, 0), 0], [(0, nr - 1, nc - 1), 1]]
        }
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6_exe, sim_ws=ws, **simkwargs
    )
    if simnamefilekwargs is not None:
        for k in simnamefilekwargs:
            sim.name_file.__setattr__(k, simnamefilekwargs[k])
    tdis = flopy.mf6.ModflowTdis(sim, **tdiskwargs)
    gwf = flopy.mf6.ModflowGwf(sim, **gwfkwargs)
    ims = flopy.mf6.ModflowIms(sim, **imskwargs)
    if diskwargs is not None:
        dis = flopy.mf6.ModflowGwfdis(gwf, **diskwargs)
    elif disukwargs is not None:
        disu = flopy.mf6.ModflowGwfdisu(gwf, **disukwargs)
    ic = flopy.mf6.ModflowGwfic(gwf, **ickwargs)
    npf = flopy.mf6.ModflowGwfnpf(gwf, **npfkwargs)
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf, **chdkwargs)
    return sim


def test_simple_model_success():
    # test a simple model to make sure it runs and terminates correctly
    ws = f"{testdir}_sim0"
    sim = get_minimal_gwf_simulation(ws)
    sim.write_simulation()
    returncode, buff = run_mf6([mf6_exe], ws)
    assert returncode == 0, "mf6 failed for simple model."

    final_message = "Normal termination of simulation."
    failure_message = f'mf6 did not terminate with "{final_message}"'
    assert final_message in buff[-1], failure_message
    if teardown_test:
        shutil.rmtree(ws, ignore_errors=True)
    return


def test_empty_folder():
    with pytest.raises(RuntimeError):
        # make sure mf6 fails when there is no simulation name file
        err_str = "mf6: mfsim.nam is not present in working directory."
        run_mf6_error(testdir, err_str)


def test_sim_errors():
    with pytest.raises(RuntimeError):
        # verify that the correct number of errors are reported
        ws = f"{testdir}_sim1"
        chdkwargs = {}
        chdkwargs["stress_period_data"] = {
            0: [[(0, 0, 0), 0.0] for i in range(10)]
        }
        sim = get_minimal_gwf_simulation(ws, chdkwargs=chdkwargs)
        sim.write_simulation()
        err_str = ["1. Cell is already a constant head ((1,1,1))."]
        run_mf6_error(ws, err_str)


def test_sim_maxerrors():
    with pytest.raises(RuntimeError):
        # verify that the maxerrors keyword gives the correct error output
        ws = f"{testdir}_sim2"
        simnamefilekwargs = {}
        simnamefilekwargs["maxerrors"] = 5
        chdkwargs = {}
        chdkwargs["stress_period_data"] = {
            0: [[(0, 0, 0), 0.0] for i in range(10)]
        }
        sim = get_minimal_gwf_simulation(
            ws, simnamefilekwargs=simnamefilekwargs, chdkwargs=chdkwargs
        )
        sim.write_simulation()
        err_str = [
            "5. Cell is already a constant head ((1,1,1)).",
            "5 additional errors detected but not printed.",
            "UNIT ERROR REPORT:",
            "1. ERROR OCCURRED WHILE READING FILE 'test.chd'",
        ]
        run_mf6_error(ws, err_str)


def test_disu_errors():
    with pytest.raises(RuntimeError):
        from disu_util import get_disu_kwargs

        ws = f"{testdir}_sim3"
        disukwargs = get_disu_kwargs(
            3, 3, 3, np.ones(3), np.ones(3), 0, [-1, -2, -3]
        )
        top = disukwargs["top"]
        bot = disukwargs["bot"]
        top[9] = 2.0
        bot[9] = 1.0
        sim = get_minimal_gwf_simulation(
            ws, disukwargs=disukwargs, chdkwargs={"stress_period_data": [[]]}
        )
        sim.write_simulation()
        err_str = [
            "1. Top elevation (    2.00000    ) for cell 10 is above bottom elevation (",
            "-1.00000    ) for cell 1. Based on node numbering rules cell 10 must be",
            "below cell 1.",
            "UNIT ERROR REPORT:"
            "1. ERROR OCCURRED WHILE READING FILE './test.disu'",
        ]
        run_mf6_error(ws, err_str)


def test_solver_fail():
    with pytest.raises(RuntimeError):
        # test failed to converge
        ws = f"{testdir}_sim4"
        imskwargs = {"inner_maximum": 1, "outer_maximum": 2}
        sim = get_minimal_gwf_simulation(ws, imskwargs=imskwargs)
        sim.write_simulation()
        err_str = [
            "Simulation convergence failure occurred 1 time(s).",
            "Premature termination of simulation.",
        ]
        run_mf6_error(ws, err_str)


def test_fail_continue_success():
    # test continue but failed to converge
    ws = f"{testdir}_sim5"
    tdiskwargs = {"nper": 1, "perioddata": [(10.0, 10, 1.0)]}
    imskwargs = {"inner_maximum": 1, "outer_maximum": 2}
    sim = get_minimal_gwf_simulation(
        ws, imskwargs=imskwargs, tdiskwargs=tdiskwargs
    )
    sim.name_file.continue_ = True
    sim.write_simulation()
    returncode, buff = run_mf6([mf6_exe], ws)
    assert returncode == 0, "mf6 failed for simple model."

    final_message = "Simulation convergence failure occurred 10 time(s)."
    failure_message = f'mf6 did not terminate with "{final_message}"'
    assert final_message in buff[0], failure_message

    final_message = "Normal termination of simulation."
    failure_message = f'mf6 did not terminate with "{final_message}"'
    assert final_message in buff[0], failure_message

    if teardown_test:
        shutil.rmtree(ws, ignore_errors=True)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    test_empty_folder()
    test_simple_model_success()
    test_sim_errors()
    test_sim_maxerrors()
    test_disu_errors()
    test_solver_fail()
    test_fail_continue_success()
