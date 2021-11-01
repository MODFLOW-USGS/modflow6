import os
import pytest
import sys
import shutil
import subprocess

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

import targets

mf6_exe = os.path.abspath(targets.target_dict["mf6"])
name = "gwf_ret_codes01"
ws = os.path.join("temp", name)
app = "mf6"
if sys.platform.lower() == "win32":
    app += ".exe"


def run_mf6(argv, ws):
    buff = []
    proc = subprocess.Popen(
        argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=ws
    )
    result, error = proc.communicate()
    if result is not None:
        c = result.decode("utf-8")
        c = c.rstrip("\r\n")
        print("{}".format(c))
        buff.append(c)

    return proc.returncode, buff


def get_sim(ws, idomain, continue_flag=False, nouter=500):
    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = [(1.0, 1, 1.0)]

    # spatial discretization data
    nlay, nrow, ncol = 1, 10, 10
    delr, delc = 1000.0, 1000.0
    top = 0.0
    botm = [-100]
    strt = 0.0

    # calculate hk
    hk = 1.0

    # solver options
    ninner = 300
    hclose, rclose, relax = 1e-9, 1e-6, 1.0
    newtonoptions = "NEWTON"
    imsla = "BICGSTAB"

    # build MODFLOW 6 files
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name=mf6_exe,
        sim_ws=ws,
        continue_=continue_flag,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=name, newtonoptions=newtonoptions, save_flows=True
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration=imsla,
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=False, icelltype=1, k=hk)

    # constant head
    if idomain > 0:
        c6 = []
        ccol = [1, ncol - 1]
        for j in ccol:
            c6.append([(0, nrow - 1, j), strt])
        c6 = [[0, 0, 0, 1.0], [0, nrow - 1, ncol - 1, 0.0]]
        cd6 = {0: c6}
        maxchd = len(cd6[0])
        chd = flopy.mf6.ModflowGwfchd(
            gwf, stress_period_data=cd6, maxbound=maxchd
        )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(name),
        head_filerecord="{}.hds".format(name),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "ALL")],
    )

    # write the input files
    sim.write_simulation()
    return sim


def normal_termination():
    # get the simulation
    sim = get_sim(ws, idomain=1)

    # write the input files
    sim.write_simulation()

    # run the simulation
    returncode, buff = run_mf6([mf6_exe], ws)
    msg = "could not run {}".format(sim.name)
    if returncode != 0:
        msg = (
            "The run should have been successful but it terminated "
            "with non-zero returncode"
        )
        raise ValueError(msg)

    return


def converge_fail_continue():
    # get the simulation
    sim = get_sim(ws, idomain=1, continue_flag=True, nouter=1)

    # write the input files
    sim.write_simulation()

    # run the simulation
    returncode, buff = run_mf6([mf6_exe], ws)
    msg = (
        "The run should have been successful even though it failed, because"
        " the continue flag was set.  But a non-zero error code was "
        "found: {}".format(returncode)
    )
    assert returncode == 0, msg
    return


def converge_fail_nocontinue():
    with pytest.raises(RuntimeError):
        # get the simulation
        sim = get_sim(ws, idomain=1, continue_flag=False, nouter=1)

        # write the input files
        sim.write_simulation()

        # run the simulation
        returncode, buff = run_mf6([mf6_exe], ws)
        msg = "This run should fail with a returncode of 1"
        if returncode == 1:
            raise RuntimeError(msg)


def idomain_runtime_error():
    with pytest.raises(RuntimeError):
        # get the simulation
        sim = get_sim(ws, idomain=0)

        # write the input files
        sim.write_simulation()

        # run the simulation
        returncode, buff = run_mf6([mf6_exe], ws)
        msg = "could not run {}".format(sim.name)
        if returncode != 0:
            err_str = "IDOMAIN ARRAY HAS SOME VALUES GREATER THAN ZERO"
            err = any(err_str in s for s in buff)
            if err:
                raise RuntimeError(msg)
            else:
                msg += " but IDOMAIN ARRAY ERROR not returned"
                raise ValueError(msg)


def unknown_keyword_error():
    with pytest.raises(RuntimeError):
        returncode, buff = run_mf6([mf6_exe, "--unknown_keyword"], ws)
        msg = "could not run {}".format("unknown_keyword")
        if returncode != 0:
            err_str = "{}: illegal option".format(app)
            err = any(err_str in s for s in buff)
            if err:
                raise RuntimeError(msg)
            else:
                msg += " but {} not returned".format(err_str)
                raise ValueError(msg)


def run_argv(arg, return_str):
    returncode, buff = run_mf6([mf6_exe, arg], ws)
    if returncode == 0:
        found_str = any(return_str in s for s in buff)
        if not found_str:
            msg = "{} keyword did not return {}".format(arg, return_str)
            raise ValueError(msg)
    else:
        msg = "could not run with command line argument {}".format(arg)
        raise RuntimeError(msg)


def help_argv():
    for arg in ["-h", "--help", "-?"]:
        return_str = "{} [options]     retrieve program information".format(
            app
        )
        run_argv(arg, return_str)


def version_argv():
    for arg in ["-v", "--version"]:
        return_str = "{}: 6".format(app)
        run_argv(arg, return_str)


def develop_argv():
    for arg in ["-dev", "--develop"]:
        return_str = "{}: develop version".format(app)
        run_argv(arg, return_str)


def compiler_argv():
    for arg in ["-c", "--compiler"]:
        return_str = "{}: MODFLOW 6 compiled".format(app)
        run_argv(arg, return_str)


def clean_sim():
    print("Cleaning up")
    shutil.rmtree(ws)


def test_main():
    idomain_runtime_error()
    unknown_keyword_error()
    normal_termination()
    converge_fail_nocontinue()
    help_argv()
    version_argv()
    develop_argv()
    compiler_argv()
    clean_sim()


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    test_main()
