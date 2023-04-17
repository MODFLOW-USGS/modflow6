import os

import flopy
import pytest
from flopy.utils.compare import eval_bud_diff
from framework import TestFramework
from simulation import TestSimulation

paktest = "ims"
ex = ["ims_rcm"]

# spatial discretization data
nlay, nrow, ncol = 2, 5, 30
delr, delc = 100.0, 100.0
top = 0.0
botm = [-10.0, -20.0]
strt = 0.0
chd_left = 10.0
chd_right = 5.0


def build_model(idx, ws):
    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = [(1.0, 1, 1.0)]

    # build MODFLOW 6 files
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="seconds",
        nper=nper,
        perioddata=tdis_rc,
    )

    if not ws.endswith("mf6"):
        reordering_method = "rcm"
    else:
        reordering_method = None

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        reordering_method=reordering_method,
        preconditioner_levels=10,
        preconditioner_drop_tolerance=1e-6,
        outer_dvclose=1e-9,
        outer_maximum=100,
        inner_dvclose=1e-12,
        inner_maximum=100,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        length_units="meters",
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf)

    # chd files
    # chd data
    spd = [[(0, i, 0), chd_left] for i in range(nrow)]
    spd += [[(0, i, ncol - 1), chd_right] for i in range(nrow)]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf, stress_period_data=spd, pname="chd-1"
    )

    # output control
    hdspth = f"{name}.hds"
    budpth = f"{name}.cbc"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=hdspth,
        budget_filerecord=budpth,
        printrecord=[
            ("BUDGET", "ALL"),
        ],
        saverecord=[
            ("BUDGET", "ALL"),
            ("HEAD", "ALL"),
        ],
    )

    return sim


def build_models(idx, base_ws):
    sim = build_model(idx, base_ws)

    ws = os.path.join(base_ws, "mf6")
    mc = build_model(idx, ws)

    return sim, mc


def eval_flows(sim):
    name = sim.name
    print("evaluating flow results..." f"({name})")

    fpth = os.path.join(sim.simpath, f"{name}.dis.grb")
    ia = flopy.mf6.utils.MfGrdFile(fpth).ia

    fpth = os.path.join(sim.simpath, f"{name}.cbc")
    b0 = flopy.utils.CellBudgetFile(fpth, precision="double")

    fpth = os.path.join(sim.simpath, "mf6", f"{name}.cbc")
    b1 = flopy.utils.CellBudgetFile(fpth, precision="double")

    fpth = os.path.join(sim.simpath, f"{name}.cbc.cmp.out")
    eval_bud_diff(fpth, b0, b1, ia=ia)

    # close the budget files
    b0.close()
    b1.close()


@pytest.mark.parametrize(
    "name",
    ex,
)
def test_mf6model(name, function_tmpdir, targets):
    ws = str(function_tmpdir)
    test = TestFramework()
    test.build(build_models, 0, ws)
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_flows,
            idxsim=0,
        ),
        ws,
    )
