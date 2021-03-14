import os
import numpy as np

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

from framework import testing_framework, running_on_CI
from simulation import Simulation

paktest = "drn"
budtol = 1e-2

ex = ["drn_ddrn02a"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"

# run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
# spatial discretization
nlay, nrow, ncol = 1, 1, 1
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 1000.0, 1000.0
top = 0.0
botm = [-10.0]

# temporal discretization
nper = 1
nyears = 3.0
period_data = [(nyears * 365.25, 100, 1.05)]

strt = 1.0
kh, kv, sy, ss = 1.0, 0.01, 0.1, 1e-5

# drain data
delev, ddrn = -0.5, 1.0
dcond = kv * delc * delr / ddrn
drn_spd = {0: [[(0, 0, 0), delev, dcond, ddrn]]}
drn_obs = {"drn_obs.csv": [("d1_1_1", "DRN", (0, 0, 0))]}

# uzf data
uzf_pd = [[0, (0, 0, 0), 1, 0, ddrn, kv, 0.05, 0.35, 0.1, 4.0]]
uzf_obs = {"uzf_obs.csv": [("d1_1_1", "UZF-GWD", (0, 0, 0))]}


def build_model(ws, name, uzf=False):
    hdsfile = "{}.hds".format(name)

    # build the model
    sim = flopy.mf6.MFSimulation(sim_name=name, exe_name="mf6", sim_ws=ws)
    tdis = flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=period_data)
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        linear_acceleration="BICGSTAB",
        outer_maximum=200,
        inner_maximum=200,
        outer_dvclose=1e-6,
        inner_dvclose=1e-9,
        rcloserecord=[0.01, "strict"],
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, newtonoptions="")
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )
    npf = flopy.mf6.ModflowGwfnpf(gwf, k=kh, icelltype=1)
    sto = flopy.mf6.ModflowGwfsto(
        gwf, sy=sy, ss=ss, transient={0: True}, iconvert=1
    )
    if uzf:
        uzf = flopy.mf6.ModflowGwfuzf(
            gwf, simulate_gwseep=True, packagedata=uzf_pd, print_input=True
        )
        uzf.obs.initialize(
            filename="{}.uzf.obs".format(name),
            digits=20,
            print_input=True,
            continuous=uzf_obs,
        )
    else:
        drn = flopy.mf6.ModflowGwfdrn(
            gwf,
            auxiliary=["ddrn"],
            auxdepthname="ddrn",
            stress_period_data=drn_spd,
            print_input=True,
        )
        drn.obs.initialize(
            filename="{}.drn.obs".format(name),
            digits=20,
            print_input=True,
            continuous=drn_obs,
        )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=hdsfile,
        saverecord=[("HEAD", "ALL")],
        printrecord=[("BUDGET", "ALL")],
    )

    return sim


def get_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = build_model(ws, name)

    # build MODFLOW 6 files with UZF package
    ws = os.path.join(dir, "mf6")
    mc = build_model(ws, name, uzf=True)

    return sim, mc


def eval_disch(sim):
    print("evaluating drain discharge and uzf discharge to land surface...")

    # MODFLOW 6 drain discharge results
    fpth = os.path.join(sim.simpath, "drn_obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # MODFLOW 6 uzf discharge results
    fpth = os.path.join(sim.simpath, "mf6", "uzf_obs.csv")
    try:
        tc0 = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # calculate maximum absolute error
    diff = tc["D1_1_1"] - tc0["D1_1_1"]
    diffmax = np.abs(diff).max()
    dtol = 1e-6
    msg = "maximum absolute discharge difference ({}) ".format(diffmax)

    # write summary
    fpth = os.path.join(
        sim.simpath, "{}.disc.cmp.out".format(os.path.basename(sim.name))
    )
    f = open(fpth, "w")
    line = "{:>15s}".format("TOTIM")
    line += " {:>15s}".format("DRN")
    line += " {:>15s}".format("UZF")
    line += " {:>15s}".format("DIFF")
    f.write(line + "\n")
    for i in range(diff.shape[0]):
        line = "{:15g}".format(tc0["time"][i])
        line += " {:15g}".format(tc["D1_1_1"][i])
        line += " {:15g}".format(tc0["D1_1_1"][i])
        line += " {:15g}".format(diff[i])
        f.write(line + "\n")
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += "exceeds {}".format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_simulation()
    return


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
            dir, exfunc=eval_disch, exe_dict=r_exe, idxsim=idx
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
            dir, exfunc=eval_disch, exe_dict=replace_exe, idxsim=idx
        )
        test.run_mf6(sim)
    return


# use python testmf6_drn_ddrn01.py
if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
