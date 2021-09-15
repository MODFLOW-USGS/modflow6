"""
MODFLOW 6 Autotest
Test the SSM FILEINPUT option for specifying source and sink
concentrations.

"""

import os
import numpy as np

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = ["ssm04"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

nlay, nrow, ncol = 3, 5, 5
idomain_lay0 = [
    [1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1],
    [1, 1, 0, 1, 1],
    [1, 1, 0, 1, 1],
    [1, 1, 1, 1, 1],
    ]
idomain = np.ones((nlay, nrow, ncol), dtype=int)
idomain[0, :, :] = np.array(idomain_lay0)

def get_model(idx, dir):
    perlen = [5.0]
    nstp = [5]
    tsmult = [1.0]
    nper = len(perlen)
    delr = 1.0
    delc = 1.0
    top = 4.0
    botm = [3., 2., 1.]
    strt = 4.0
    hk = 1.0
    laytyp = 0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

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
    gwfname = "gwf_" + name
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        save_flows=True,
    )

    # create iterative model solution and register the gwf model with it
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwfname),
    )
    sim.register_ims_package(imsgwf, [gwf.name])

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
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=laytyp,
        k=hk,
        save_specific_discharge=True,
    )

    # chd files
    spd = [[(nlay - 1, nrow - 1, ncol - 1), 4.]]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf,
        print_flows=True,
        maxbound=len(spd),
        stress_period_data=spd,
        pname="CHD-1",
    )

    # list based recharge
    idxrow, idxcol = np.where(idomain[0] == 1)
    recharge_rate = np.arange(nrow * ncol).reshape((nrow, ncol))
    spd = []
    for i, j in zip(idxrow, idxcol):
        spd.append([(0, i, j), recharge_rate[i, j]])
    rch1 = flopy.mf6.modflow.ModflowGwfrch(
        gwf,
        print_flows=True,
        maxbound=len(spd),
        stress_period_data=spd,
        pname="RCH-1",
    )

    # array-based rch files
    rch2 = flopy.mf6.ModflowGwfrcha(
        gwf,
        print_flows=True,
        recharge= recharge_rate,
        pname="RCH-2",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(gwfname),
        head_filerecord="{}.hds".format(gwfname),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
        model_nam_file="{}.nam".format(gwtname),
    )
    gwt.name_file.save_flows = True

    # create iterative model solution and register the gwt model with it
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwtname),
    )
    sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(
        gwt,
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
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0)

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt)

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.1)

    # ssm package
    sourcerecarray = [()]
    fileinput = [("RCH-1", f"{gwtname}.rch1.spc"),
                 ("RCH-2", f"{gwtname}.rch2.spc"),]
    ssm = flopy.mf6.ModflowGwtssm(gwt,
                                  print_flows=True,
                                  sources=sourcerecarray,
                                  fileinput=fileinput)

    # spc package for RCH-1
    idxrow, idxcol = np.where(idomain[0] == 1)
    recharge_concentration = np.arange(nrow * ncol).reshape((nrow, ncol))
    pd = []
    for ipos, (i, j) in enumerate (zip(idxrow, idxcol)):
        pd.append([ipos, "CONCENTRATION", recharge_concentration[i, j]])
    spc1 = flopy.mf6.ModflowUtlspc(
        gwt, perioddata=pd, maxbound=len(pd), filename=f"{gwtname}.rch1.spc",
    )

    # spc package for RCH-2
    idxrow, idxcol = np.where(idomain[0] == 1)
    recharge_concentration = np.arange(nrow * ncol).reshape((nrow, ncol))
    pd = []
    for ipos, (i, j) in enumerate (zip(idxrow, idxcol)):
        pd.append([ipos, "CONCENTRATION", recharge_concentration[i, j]])
    spc2 = flopy.mf6.ModflowUtlspca(
        gwt, concentration=recharge_concentration,
        filename=f"{gwtname}.rch2.spc",
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord="{}.cbc".format(gwtname),
        concentration_filerecord="{}.ucn".format(gwtname),
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    obs_data = {
        f"{gwtname}.obs.csv": [
            ("(1-1-1)", "CONCENTRATION", (0, 0, 0)),
            ("(1-5-5)", "CONCENTRATION", (nlay - 1, nrow - 1, ncol - 1)),
        ],
    }

    obs_package = flopy.mf6.ModflowUtlobs(
        gwt,
        pname=f"{gwtname}.obs",
        digits=10,
        print_input=True,
        continuous=obs_data,
    )

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename="{}.gwfgwt".format(name),
    )

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name

    # load concentration file
    fpth = os.path.join(sim.simpath, "{}.ucn".format(gwtname))
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        conc = cobj.get_data()
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    # load transport budget file
    fpth = os.path.join(sim.simpath, "{}.cbc".format(gwtname))
    try:
        bobj = flopy.utils.CellBudgetFile(
            fpth, precision="double",
        )
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    ssmbudall = bobj.get_data(text="SOURCE-SINK MIX")
    print(ssmbudall)
    for ssmbud in ssmbudall:

        node, node2, q = ssmbud[0]
        assert node == 25, "node location for well must be 25 (GWT cell 25)"
        assert node2 == 1, "node2 location for well must be 1 (first well)"
        assert q == 100., "mass flux for well must be 100."

        node, node2, q = ssmbud[1]
        assert node == 1, "node location for chd must be 1 (first GWT cell)"
        assert node2 == 1, "node2 location for chd must be 1 (first chd)"
        assert q < 0., "mass flux for chd must be less than zero"

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_transport, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
