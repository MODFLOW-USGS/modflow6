import os
import numpy as np

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework, running_on_CI
from simulation import Simulation

ex = ["gwf_sto03a", "gwf_sto03b",]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

newton = (False, True,)

cmppth = "mf6"

ddir = "data"

## run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# use default executable
replace_exe = None

htol = [None for idx in range(len(exdirs))]
dtol = 1e-3
budtol = 1e-2
ur_gamma = 0.95

bud_lst = ("STO-SS_IN", "STO-SS_OUT",)

# static model data
# temporal discretization
nper = 6
perlen = [1.0 for i in range(nper)]
nstp = [25 for i in range(nper)]
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
zelev = (0., -100.,)
strt = zelev[-1] + 1e-7
cmp_offset = 15999.1
obsname = "H1"

# hydraulic properties
hk = 1.
ib = 1
laytyp = 1
ss = 1e-5
sy = 0.

# solver options
nouter, ninner = 500, 300
hclose, rclose, relax = 1e-9, 1e-6, 1.0

# pumping well data
absrate = 1.1 * ss * (zelev[-2] - zelev[-1]) * 125.
well_spd = {}
for idx in range(nper):
    if idx % 2 == 0:
        mult = 1.
    else:
        mult = -1.
    well_spd[idx] = [[0, 0, 0, mult * absrate]]

def build_model(name, ws, newton_bool, offset=0.):
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
        newtonoptions = "UNDER_RELAXATION"
    else:
        linear_acceleration = "CG"
        newtonoptions = None

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="SIMPLE",
        under_relaxation_gamma=ur_gamma,
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
        filename='{}.obs'.format(name),
        digits=10,
        print_input=True,
        continuous={
            "head.obs.csv": [
                (obsname, "HEAD", (0, 0, 0))
            ]
        }
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
        budget_filerecord="{}.cbc".format(name),
        saverecord=[("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    return sim


# variant SUB package problem 3
def get_model(idx, dir):
    name = ex[idx]
    ws = dir

    # build model with no offset
    sim = build_model(name, ws, newton_bool=newton[idx])

    # build model with offset
    ws = os.path.join(dir, cmppth)
    mc = build_model(name, ws, newton_bool=newton[idx], offset=cmp_offset)
    return sim, mc


def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        mc.write_simulation()
    return


def eval_sto(sim):
    print("evaluating head differences...")
    fpth = os.path.join(
        sim.simpath,
        "head.obs.csv"
    )
    base_obs = flopy.utils.Mf6Obs(fpth).get_data(obsname="H1")[obsname]

    fpth = os.path.join(
        sim.simpath,
        cmppth,
        "head.obs.csv"
    )
    offset_obs = flopy.utils.Mf6Obs(fpth).get_data(obsname=obsname)[obsname]
    offset_obs -= cmp_offset

    msg = "head differences exceed tolerance when offset removed " + \
          "- maximum difference {}".format((base_obs - offset_obs).max())
    assert np.allclose(base_obs, offset_obs, atol=1e-6), msg

    print("evaluating storage...")
    name = ex[sim.idxsim]
    fpth = os.path.join(
        sim.simpath,
        "{}.cbc".format(name)
    )
    base_cbc = flopy.utils.CellBudgetFile(fpth, precision="double")
    fpth = os.path.join(
        sim.simpath,
        cmppth,
        "{}.cbc".format(name)
    )
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

    assert max_diff.sum() == 0., "simulated storage is not the same"

    return


# - No need to change any code below
def test_mf6model():

    # determine if running on Travis or GitHub actions
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        if is_CI and not continuous_integration[idx]:
            continue
        yield test.run_mf6, Simulation(
            dir, exfunc=eval_sto, exe_dict=r_exe, htol=htol[idx], idxsim=idx
        )

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
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
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
