import os
import sys
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

from framework import testing_framework
from simulation import Simulation

ex = ["maw02"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"

budtol = 1e-2
bud_lst = ["GWF_IN", "GWF_OUT", "RATE_IN", "RATE_OUT"]

# spatial and temporal data
nlay, nrow, ncol = 1, 1, 3
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
nper = 5
perlen = nper * [1.0]
nstp = nper * [1]
tsmult = nper * [1.0]
steady = nper * [True]
lenx = 300.0
delr = delc = lenx / float(nrow)
botm = [0.0]
strt = 100.0
hnoflo = 1e30
hdry = -1e30
hk = 1.0

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 1.0
krylov = ["CG"]

tdis_rc = []
for i in range(nper):
    tdis_rc.append((perlen[i], nstp[i], tsmult[i]))


def get_model(idx, dir):

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwf = flopy.mf6.MFModel(
        sim,
        model_type="gwf6",
        modelname=name,
        model_nam_file="{}.nam".format(name),
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
        linear_acceleration=krylov[idx],
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
        top=100.0,
        botm=0.0,
        idomain=1,
        filename="{}.dis".format(name),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename="{}.ic".format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=True,
        icelltype=1,
        k=hk,
        k33=hk,
        filename="{}.npf".format(name),
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=True,
        iconvert=1,
        ss=0.0,
        sy=0.1,
        steady_state={0: True},
        # transient={1: False},
        filename="{}.sto".format(name),
    )

    # chd files
    chdlist0 = []
    chdlist0.append([(0, 0, 0), 100.0])
    chdlist0.append([(0, 0, 2), 100.0])

    chdlist1 = []
    chdlist1.append([(0, 0, 0), 25.0])
    chdlist1.append([(0, 0, 2), 25.0])

    chdspdict = {0: chdlist0, 1: chdlist1, 2: chdlist0}
    chd = flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=chdspdict,
        save_flows=False,
        filename="{}.chd".format(name),
    )

    # MAW
    opth = "{}.maw.obs".format(name)
    wellbottom = 0.0
    wellrecarray = [
        [0, 0.1, wellbottom, 100.0, "THIEM", 1],
        [1, 0.1, wellbottom, 100.0, "THIEM", 1],
    ]
    wellconnectionsrecarray = [
        [0, 0, (0, 0, 1), 100.0, wellbottom, 1.0, 0.1],
        [1, 0, (0, 0, 1), 100.0, wellbottom, 1.0, 0.1],
    ]
    wellperiodrecarray = {
        0: [
            [0, "rate", -20.0],
            [0, "status", "inactive"],
            [0, "rate_scaling", 1.0, 15.0],
            [1, "rate", -30.0],
            [1, "status", "inactive"],
            [1, "rate_scaling", 5.0, 15.0],
        ],
        1: [
            [0, "rate", -110.0],
            [0, "status", "active"],
            [1, "rate", -130.0],
            [1, "status", "active"],
        ],
        3: [[0, "status", "inactive"]],
        4: [[0, "status", "active"]],
    }
    mawo_dict = {}
    mawo_dict["maw_obs.csv"] = [("mh1", "head", 1)]
    maw = flopy.mf6.ModflowGwfmaw(
        gwf,
        filename="{}.maw".format(name),
        budget_filerecord="{}.maw.cbc".format(name),
        print_input=True,
        print_head=True,
        print_flows=True,
        save_flows=True,
        observations=mawo_dict,
        packagedata=wellrecarray,
        connectiondata=wellconnectionsrecarray,
        perioddata=wellperiodrecarray,
        pname="MAW-1",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(name),
        head_filerecord="{}.hds".format(name),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        filename="{}.oc".format(name),
    )

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def eval_maw(sim):
    print("evaluating MAW budgets...")

    # get results from listing file
    fpth = os.path.join(
        sim.simpath, "{}.lst".format(os.path.basename(sim.name))
    )
    budl = flopy.utils.Mf6ListBudget(
        fpth, budgetkey="MAW-1 BUDGET FOR ENTIRE MODEL AT END OF TIME STEP"
    )
    names = list(bud_lst)
    d0 = budl.get_budget(names=names)[0]
    dtype = d0.dtype
    nbud = d0.shape[0]

    # get results from cbc file
    cbc_bud = ["GWF", "RATE"]
    d = np.recarray(nbud, dtype=dtype)
    for key in bud_lst:
        d[key] = 0.0
    fpth = os.path.join(
        sim.simpath, "{}.maw.cbc".format(os.path.basename(sim.name))
    )
    cobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    kk = cobj.get_kstpkper()
    times = cobj.get_times()
    cbc_vals = []
    for idx, (k, t) in enumerate(zip(kk, times)):
        for text in cbc_bud:
            qin = 0.0
            qout = 0.0
            v = cobj.get_data(kstpkper=k, text=text)[0]
            if isinstance(v, np.recarray):
                vt = np.zeros(size3d, dtype=float)
                wq = []
                for jdx, node in enumerate(v["node"]):
                    vt[node - 1] += v["q"][jdx]
                    wq.append(v["q"][jdx])
                v = vt.reshape(shape3d)
                if text == cbc_bud[-1]:
                    cbc_vals.append(wq)
            for kk in range(v.shape[0]):
                for ii in range(v.shape[1]):
                    for jj in range(v.shape[2]):
                        vv = v[kk, ii, jj]
                        if vv < 0.0:
                            qout -= vv
                        else:
                            qin += vv
            d["totim"][idx] = t
            d["time_step"][idx] = k[0]
            d["stress_period"] = k[1]
            key = "{}_IN".format(text)
            d[key][idx] = qin
            key = "{}_OUT".format(text)
            d[key][idx] = qout

    maw_vals = [
        [0.000, 0.000],
        [-106.11303563809453, -96.22598985147631],
        [-110.000, -130.000],
        [0.0, -130.000],
        [-110.000, -130.000],
    ]

    # evaluate if well rates in cbc file are equal to expected values
    diffv = []
    for ovs, svs in zip(maw_vals, cbc_vals):
        for ov, sv in zip(ovs, svs):
            diffv.append(ov - sv)
    diffv = np.abs(np.array(diffv)).max()
    msg = "\nmaximum absolute maw rate difference     ({})\n".format(diffv)

    # calculate difference between water budget items in the lst and cbc files
    diff = np.zeros((nbud, len(bud_lst)), dtype=float)
    for idx, key in enumerate(bud_lst):
        diff[:, idx] = d0[key] - d[key]
    diffmax = np.abs(diff).max()
    msg += "maximum absolute total-budget difference ({}) ".format(diffmax)

    # write summary
    fpth = os.path.join(
        sim.simpath, "{}.bud.cmp.out".format(os.path.basename(sim.name))
    )
    f = open(fpth, "w")
    for i in range(diff.shape[0]):
        if i == 0:
            line = "{:>10s}".format("TIME")
            for idx, key in enumerate(bud_lst):
                line += "{:>25s}".format(key + "_LST")
                line += "{:>25s}".format(key + "_CBC")
                line += "{:>25s}".format(key + "_DIF")
            f.write(line + "\n")
        line = "{:10g}".format(d["totim"][i])
        for idx, key in enumerate(bud_lst):
            line += "{:25g}".format(d0[key][i])
            line += "{:25g}".format(d[key][i])
            line += "{:25g}".format(diff[i, idx])
        f.write(line + "\n")
    f.close()

    if diffmax > budtol or diffv > budtol:
        sim.success = False
        msg += "\n...exceeds {}".format(budtol)
        assert diffmax < budtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_maw, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_maw, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
