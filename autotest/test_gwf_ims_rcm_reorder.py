import os

import pytest

from budget_file_compare import eval_bud_diff

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

paktest = "ims"

ex = [
    "ims_rcm",
]
exdirs = [os.path.join("temp", s) for s in ex]

# spatial discretization data
nlay, nrow, ncol = 2, 5, 30
delr, delc = 100.0, 100.0
top = 0.0
botm = [-10.0, -20.0]
strt = 0.0
chd_left = 10.0
chd_right = 5.0

#
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
    idx = sim.idxsim
    name = ex[idx]
    print("evaluating flow results..." f"({name})")

    fpth = os.path.join(exdirs[idx], f"{name}.dis.grb")
    ia = flopy.mf6.utils.MfGrdFile(fpth).ia

    fpth = os.path.join(exdirs[idx], f"{name}.cbc")
    b0 = flopy.utils.CellBudgetFile(fpth, precision="double")

    fpth = os.path.join(exdirs[idx], "mf6", f"{name}.cbc")
    b1 = flopy.utils.CellBudgetFile(fpth, precision="double")

    fpth = os.path.join(exdirs[idx], f"{name}.cbc.cmp.out")
    eval_bud_diff(fpth, b0, b1, ia=ia)

    # close the budget files
    b0.close()
    b1.close()


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, exdir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, exdir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_models, idx, exdir)

    # run the test models
    test.run_mf6(
        Simulation(
            exdir,
            exfunc=eval_flows,
            idxsim=idx,
        )
    )


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test models
    for idx, exdir in enumerate(exdirs):
        test.build_mf6_models(build_models, idx, exdir)

        sim = Simulation(
            exdir,
            exfunc=eval_flows,
            idxsim=idx,
        )
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
