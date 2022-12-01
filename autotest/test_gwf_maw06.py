# Test maw package ability to equalize and the flow correction.
# maw_06a - well start at .25, aquifer starts at 2
# maw_06b - well starts at 2, aquifer starts at .25

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

from framework import testing_framework
from simulation import Simulation

ex = ["maw_06a", "maw_06b"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

nlay = 2
nrow = 1
ncol = 1

delc = 1.0
delr = 1.0
gwfarea = delr * delc

top = 2.0
bot = 0.0
aqthick = top - bot
dz = aqthick / float(nlay)
botm = [top - dz * (k + 1) for k in range(nlay)]
ztop = [top - dz * k for k in range(nlay)]

strt_min = aqthick / 8.0
mawstrt = [
    strt_min,
    top,
]
gwfstrt = [
    top,
    strt_min,
]

Kh = 1.0
Kv = 1.0
sy = 1.0
ss = 0.0

mawarea = 1.0
mawradius = np.sqrt(mawarea / np.pi)  # .65
mawcond = Kh * delc * dz / (0.5 * delr)


def build_model(idx, dir):
    nper = 1
    perlen = [10.0]
    nstp = [100]
    tsmult = [1.005]

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    nouter, ninner = 700, 200
    hclose, rclose, relax = 1e-9, 1e-9, 1.0

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        memory_print_option="summary",
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwfname = "gwf_" + name

    newtonoptions = "NEWTON UNDER_RELAXATION"
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=gwfname, newtonoptions=newtonoptions
    )

    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="SIMPLE",
        under_relaxation_gamma=0.98,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=f"{rclose} strict",
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename=f"{gwfname}.ims",
    )

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

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=gwfstrt[idx])

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=1,
        k=Kh,
        k33=Kv,
    )

    sto = flopy.mf6.ModflowGwfsto(gwf, sy=sy, ss=ss, iconvert=1)

    mstrt = mawstrt[idx]
    mawcondeqn = "SPECIFIED"
    mawngwfnodes = nlay
    # <wellno> <radius> <bottom> <strt> <condeqn> <ngwfnodes>
    mawpackagedata = [[0, mawradius, bot, mstrt, mawcondeqn, mawngwfnodes]]
    # <wellno> <icon> <cellid(ncelldim)> <scrn_top> <scrn_bot> <hk_skin> <radius_skin>
    mawconnectiondata = [
        [0, icon, (icon, 0, 0), top, bot, mawcond, -999]
        for icon in range(nlay)
    ]
    # <wellno> <mawsetting>
    mawperioddata = [[0, "STATUS", "ACTIVE"]]
    mbin = f"{gwfname}.maw.bin"
    mbud = f"{gwfname}.maw.bud"
    maw = flopy.mf6.ModflowGwfmaw(
        gwf,
        print_input=True,
        print_head=True,
        print_flows=True,
        save_flows=True,
        flow_correction=True,
        head_filerecord=mbin,
        budget_filerecord=mbud,
        packagedata=mawpackagedata,
        connectiondata=mawconnectiondata,
        perioddata=mawperioddata,
        pname="MAW-1",
    )
    opth = f"{gwfname}.maw.obs"
    obsdata = {
        f"{gwfname}.maw.obs.csv": [
            ("whead", "head", (0,)),
        ]
    }
    maw.obs.initialize(
        filename=opth, digits=20, print_input=True, continuous=obsdata
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[
            (
                "HEAD",
                "ALL",
            ),
            (
                "BUDGET",
                "ALL",
            ),
        ],
        printrecord=[
            (
                "HEAD",
                "ALL",
            ),
            (
                "BUDGET",
                "ALL",
            ),
        ],
    )

    return sim, None


def eval_results(sim):
    print("evaluating results...")

    # calculate volume of water and make sure it is conserved
    name = ex[sim.idxsim]
    gwfname = "gwf_" + name
    fname = gwfname + ".maw.bin"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.HeadFile(fname, text="HEAD")
    stage = bobj.get_alldata().flatten()

    fname = gwfname + ".hds"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    hobj = flopy.utils.HeadFile(fname)
    head = hobj.get_alldata()

    # calculate initial volume of water in well and aquifer
    v0maw = mawstrt[sim.idxsim] * mawarea
    v0gwf = (gwfstrt[sim.idxsim] - bot) * sy * gwfarea
    v0 = v0maw + v0gwf

    print(
        "Initial volumes\n"
        + f"  Groundwater:    {v0gwf}\n"
        + f"  Well:           {v0maw}\n"
        + f"  Total:          {v0}"
    )

    # calculate current volume of water in well and aquifer and compare with
    # initial volume
    for kstp, mawstage in enumerate(stage):
        vgwf = 0.0
        for k in range(nlay):
            for j in range(ncol):
                tp = min(head[kstp, k, 0, j], ztop[k])
                dz = tp - botm[k]
                vgwf += max(0.0, dz) * sy * gwfarea
        vmaw = (stage[kstp] - bot) * mawarea
        vnow = vmaw + vgwf
        errmsg = (
            f"kstp {kstp + 1}: \n"
            + f"  Groundwater:   {vgwf}\n"
            + f"  Well:          {vmaw}\n"
            + f"  Total:         {vnow}\n"
            + f"  Initial Total: {v0}"
        )
        assert np.allclose(v0, vnow), errmsg

    print(
        f"kstp {kstp + 1}: \n"
        + f"  Groundwater:   {vgwf}\n"
        + f"  Well:          {vmaw}\n"
        + f"  Total:         {vnow}\n"
        + f"  Initial Total: {v0}"
    )

    # compare the maw-gwf flows with the gwf-maw flows
    fname = gwfname + ".maw.bud"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    mbud = flopy.utils.CellBudgetFile(fname, precision="double")
    maw_gwf = mbud.get_data(text="GWF")

    fname = gwfname + ".cbc"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    gbud = flopy.utils.CellBudgetFile(fname, precision="double")
    gwf_maw = gbud.get_data(text="MAW")

    assert len(maw_gwf) == len(gwf_maw), "number of budget records not equal"

    for istp, (ra_maw, ra_gwf) in enumerate(zip(maw_gwf, gwf_maw)):
        for i in range(ra_maw.shape[0]):
            qmaw = ra_maw[i]["q"]
            qgwf = ra_gwf[i]["q"]
            msg = f"step {istp} record {i} comparing qmaw with qgwf: {qmaw} {qgwf}"
            print(msg)
            assert np.allclose(qmaw, -qgwf), msg

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir, exfunc=eval_results, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_results, idxsim=idx)
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
