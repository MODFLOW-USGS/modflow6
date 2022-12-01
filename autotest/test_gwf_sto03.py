import os

import numpy as np
import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import running_on_CI, testing_framework
from simulation import Simulation

ex = [
    "gwf_sto03a",
    "gwf_sto03b",
]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

newton = (
    False,
    True,
)

cmppth = "mf6"

ddir = "data"

## run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# use default executable
replace_exe = None

htol = [None for idx in range(len(exdirs))]
dtol = 1e-3
budtol = 1e-2

bud_lst = (
    "STO-SS_IN",
    "STO-SS_OUT",
)

# static model data
# temporal discretization
nper = 6
perlen = [1.0 for i in range(nper)]
nstp = [50 for i in range(nper)]
tsmult = [1.1 for i in range(nper)]
tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

# spatial discretization data
nlay, nrow, ncol = 1, 1, 1
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 1.0, 1.0
area = delr * delc
zelev = (
    0.0,
    -100.0,
)
strt = zelev[-1] + 1e-7
cmp_offset = 15999.1
obsname = "H1"

# hydraulic properties
hk = 1.0
ib = 1
laytyp = 1
ss = 1e-5
sy = 0.0

# solver options
nouter, ninner = 500, 300
hclose, rclose, relax, ur_gamma = 1e-9, 1e-6, 1.0, 0.95

# pumping well data
absrate = 1.1 * ss * (zelev[-2] - zelev[-1]) * 90.0
well_spd = {}
for idx in range(nper):
    if idx % 2 == 0:
        mult = 1.0
    else:
        mult = -1.0
    well_spd[idx] = [[0, 0, 0, mult * absrate]]


def get_model(name, ws, newton_bool, offset=0.0):
    # build MODFLOW 6 files
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create iterative model solution and register the gwf model with it
    if newton_bool:
        linear_acceleration = "BICGSTAB"
        newtonoptions = "NEWTON UNDER_RELAXATION"
        gamma = 1.0
    else:
        linear_acceleration = "CG"
        newtonoptions = None
        gamma = ur_gamma

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="SIMPLE",
        under_relaxation_gamma=gamma,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration=linear_acceleration,
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        newtonoptions=newtonoptions,
        save_flows=True,
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=zelev[0] + offset,
        botm=zelev[-1] + offset,
    )

    flopy.mf6.ModflowUtlobs(
        gwf,
        filename=f"{name}.obs",
        digits=10,
        print_input=True,
        continuous={"head.obs.csv": [(obsname, "HEAD", (0, 0, 0))]},
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(
        gwf,
        strt=strt + offset,
    )

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=laytyp,
        k=hk,
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        iconvert=laytyp,
        ss=ss,
        sy=sy,
        transient={0: True},
    )

    # wel file
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        stress_period_data=well_spd,
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        saverecord=[("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    return sim


# variant SUB package problem 3
def build_model(idx, dir):
    name = ex[idx]
    ws = dir

    # build model with no offset
    sim = get_model(name, ws, newton_bool=newton[idx])

    # build model with offset
    ws = os.path.join(dir, cmppth)
    mc = get_model(name, ws, newton_bool=newton[idx], offset=cmp_offset)
    return sim, mc


def eval_hmax(fpth):
    b = flopy.utils.Mf6Obs(fpth)
    times = b.get_times()
    ctimes = np.arange(3.0, times[-1], 2.0)
    bv = np.zeros(ctimes.shape, dtype=float)
    bv[:] = b.get_data(totim=1.0)[obsname]
    sv = np.zeros(ctimes.shape, dtype=float)
    for idx, t in enumerate(ctimes):
        sv[idx] = b.get_data(totim=t)[obsname]

    msg = (
        "maximum heads in {} exceed tolerance ".format(fpth)
        + f"- maximum difference {(bv - sv).max()}"
    )
    assert np.allclose(bv, sv), msg
    return


def eval_sto(sim):
    print("evaluating head differences...")
    fpth = os.path.join(sim.simpath, "head.obs.csv")
    base_obs = flopy.utils.Mf6Obs(fpth).get_data()[obsname]

    fpth = os.path.join(sim.simpath, cmppth, "head.obs.csv")
    offset_obs = flopy.utils.Mf6Obs(fpth).get_data()[obsname]
    offset_obs -= cmp_offset

    msg = (
        "head differences exceed tolerance when offset removed "
        + f"- maximum difference {(base_obs - offset_obs).max()}"
    )
    assert np.allclose(base_obs, offset_obs, atol=1e-6), msg

    print("evaluating maximum heads...")
    fpth = os.path.join(sim.simpath, "head.obs.csv")
    eval_hmax(fpth)
    fpth = os.path.join(sim.simpath, cmppth, "head.obs.csv")
    eval_hmax(fpth)

    base_obs = flopy.utils.Mf6Obs(fpth)
    times = base_obs.get_times()
    cmp_times = np.arange(3.0, times[-1], 2.0)
    base_cmp = np.zeros(cmp_times.shape, dtype=float)
    base_cmp[:] = base_obs.get_data(totim=1.0)[obsname]
    offset_cmp = np.zeros(cmp_times.shape, dtype=float)
    for idx, t in enumerate(cmp_times):
        offset_cmp[idx] = base_obs.get_data(totim=t)[obsname]

    msg = (
        "maximum heads exceed tolerance when offset removed "
        + f"- maximum difference {(base_cmp - offset_cmp).max()}"
    )
    assert np.allclose(base_cmp, offset_cmp), msg

    print("evaluating storage...")
    name = ex[sim.idxsim]
    fpth = os.path.join(sim.simpath, f"{name}.cbc")
    base_cbc = flopy.utils.CellBudgetFile(fpth, precision="double")
    fpth = os.path.join(sim.simpath, cmppth, f"{name}.cbc")
    offset_cbc = flopy.utils.CellBudgetFile(fpth, precision="double")

    # get results from cbc file
    cbc_bud = ("STO-SS",)
    kk = base_cbc.get_kstpkper()
    times = base_cbc.get_times()
    max_diff = np.zeros(len(times), dtype=float)
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            base_v = base_cbc.get_data(totim=t, text=text)[0]
            offset_v = offset_cbc.get_data(totim=t, text=text)[0]
            if not np.allclose(base_v, offset_v):
                max_diff[idx] = np.abs(base_v - offset_v).max()

    assert max_diff.sum() == 0.0, "simulated storage is not the same"

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):

    # determine if running on CI infrastructure
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    if is_CI and not continuous_integration[idx]:
        return
    test.run_mf6(
        Simulation(
            dir, exfunc=eval_sto, exe_dict=r_exe, htol=htol[idx], idxsim=idx
        )
    )


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(
            dir,
            exfunc=eval_sto,
            exe_dict=replace_exe,
            htol=htol[idx],
            idxsim=idx,
        )
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
