"""
Test for splitting parallel MODFLOW GWF model
"""

import pathlib as pl

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = [
    "par_gwf_spl01",
]

gwf_name = "gwf"

# solver data
nouter, ninner = 100, 300
hclose, rclose, relax = 10e-9, 1e-3, 0.97


def get_model(idx, test):
    name = cases[idx]

    # parameters and spd
    # tdis
    nper = 1
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((1.0, 1, 1))

    # model spatial discretization
    nlay = 1
    nrow = 10
    ncol = 10

    # cell spacing
    delr = 100.0
    delc = 100.0

    # top/bot of the aquifer
    tops = [0.0, -10.0]

    # hydraulic conductivity
    k11 = 10.0

    # boundary stress period data
    h_left = 1.0
    h_right = 10.0

    # initial head
    h_start = 0.0

    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=test.workspace,
    )

    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        relaxation_factor=relax,
        pname="ims",
    )

    # left CHD:
    left_chd = [
        [(ilay, irow, 0), h_left]
        for irow in range(nrow)
        for ilay in range(nlay)
    ]
    chd_spd_left = {0: left_chd}

    # right CHD:
    right_chd = [
        [(ilay, irow, ncol - 1), h_right]
        for irow in range(nrow)
        for ilay in range(nlay)
    ]
    chd_spd_right = {0: right_chd}

    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwf_name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=tops[0],
        botm=tops[1 : nlay + 1],
    )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        save_flows=True,
        icelltype=0,
        k=k11,
    )
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd_left)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{gwf_name}.hds",
        budget_filerecord=f"{gwf_name}.cbc",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # split the model in two
    mfsplit = flopy.mf6.utils.Mf6Splitter(sim)

    split_array = np.zeros((nrow, ncol), dtype=int)
    split_array[:, int(ncol / 2) : ncol] = 1
    split_sim = mfsplit.split_model(split_array)
    split_sim.set_sim_path(test.workspace)

    return split_sim


def build_models(idx, test):
    sim = get_model(idx, test)
    return sim, None


def check_output(idx, test):
    pass


@pytest.mark.parallel
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        compare=None,
        parallel=True,
        ncpus=2,
    )
    test.run()
