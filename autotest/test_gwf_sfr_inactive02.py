# Test evap in SFR reaches (no interaction with gwf)

import math
import pathlib as pl

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = ["sfr-inactive02"]


def build_models(idx, test):
    # Base simulation and model name and workspace
    ws = test.workspace
    name = cases[idx]

    length_units = "m"
    time_units = "sec"

    nrow = 1
    ncol = 1
    nlay = 1
    delr = delc = 1.0
    
    nper = 2
    tds_spd = [
        (1.0, 1, 1.0),
        (1.0, 1, 1.0),
    ]

    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=ws, exe_name="mf6", version="mf6"
    )
    flopy.mf6.ModflowTdis(
        sim, 
        time_units=time_units,
        nper=nper,
        perioddata=tds_spd,
        )
    flopy.mf6.ModflowIms(
        sim,
        inner_dvclose=1e-5,
        inner_hclose=1e-6,
    )

    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
    )
    flopy.mf6.ModflowGwfdis(
        gwf,
        length_units=length_units,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=0.0,
        botm=-100.0,
    )
    flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=1,  # >0 means saturated thickness varies with computed head
    )
    flopy.mf6.ModflowGwfic(gwf, strt=1.0)
    flopy.mf6.ModflowGwfghb(gwf, stress_period_data=[((0, 0, 0), 1.0, 1e6)])

    # sfr data
    nreaches = 2
    
    # <ifno> <cellid(ncelldim)> <rlen> <rwid> <rgrd> <rtp> <rbth> <rhk> <man> <ncon> <ustrf> <ndv>
    package_data = []
    for idx in range(nreaches):
        if idx == 0:
            ustrf = 0.0
        else:
            ustrf = 1.0
        package_data.append(
            (idx, (0, 0, 0), delr, 1.0, 1e-3, 0.0, 1.0, 1.0, 0.001, 1, ustrf, 0)
        )
    connection_data = [
        (0, -1),
        (1, 0)
        ]
    
    sfr_spd = {
        0:
            [
                (0, "inflow", 1.0),
                (0, "rainfall", 1.0),
                (0, "evaporation", 1.0),
                (0, "runoff", 1.0),
                (1, "rainfall", 1.0),
                (1, "evaporation", 1.0),
                (1, "runoff", 1.0),
            ],
        1:
            [
                (1, "status", "inactive"),
            ],
        }
        
    sfr_obs = {
        f"{name}.sfr.obs.csv": [
            ("gwf", "sfr", (0,)),
            ("outflow", "ext-outflow", (0,)),
            ("depth", "depth", (0,)),
        ],
        "filename": name + ".sfr.obs",
    }

    flopy.mf6.ModflowGwfsfr(
        gwf,
        save_flows=True,
        print_stage=True,
        print_flows=True,
        print_input=True,
        stage_filerecord=f"{name}.sfr.hds",
        budget_filerecord=f"{name}.sfr.cbc",
        length_conversion=1.0,
        time_conversion=1.0,
        nreaches=nreaches,
        packagedata=package_data,
        connectiondata=connection_data,
        perioddata=sfr_spd,
        observations=sfr_obs,
    )
    
    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        saverecord=[("budget", "all")],
    )

    return sim, None


def check_output(idx, test):
    answer = np.array(
        [
            1.0,
            -0.92094535738673577,
            -0.92094535738673577,
            0.79053721667952215e-1,
        ]
    )
    obs_pth = pl.Path(f"{test.workspace}/{cases[idx]}.sfr.obs.csv")
    sim_data = flopy.utils.Mf6Obs(obs_pth).get_data()
    data_names = sim_data.dtype.names
    # for idx, name in enumerate(data_names):
    #     assert np.allclose(
    #         sim_data[name][0], answer[idx]
    #     ), f"simulated sfr {name} results do not match answer"


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
