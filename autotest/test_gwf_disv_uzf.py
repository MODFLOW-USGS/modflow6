"""
A test of DISV with UZF.  Originally created due to a possible bug in the
ASCII output file generated by UZF.  Uses quadrilateral cells.  The cells
are created from a numpy grid with cells that are 1m x 1m.  Although a DISV
grid, arrangement mimics that of a 10 row x 10 col x 5 layer DIS grid.
Inflow from infiltration on the top of the grid, flow exits via GHB
boundary on the right-hand side of the model.
"""

import os

import flopy
import flopy.utils.cvfdutil
import numpy as np
import pytest
from flopy.utils.gridutil import get_disv_kwargs
from framework import TestFramework

cases = ["disv_with_uzf"]
nlay = 5
nrow = 10
ncol = 10
ncpl = nrow * ncol
delr = 1.0
delc = 1.0
nper = 5
perlen = [10] * 5
nstp = [5] * 5
tsmult = len(perlen) * [1.0]
top = 25.0
botm = [20.0, 15.0, 10.0, 5.0, 0.0]
strt = 20
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97

# use flopy util to get disv arguments
disvkwargs = get_disv_kwargs(
    nlay,
    nrow,
    ncol,
    delr,
    delc,
    top,
    botm,
)

# Work up UZF data
iuzno = 0
cellid = 0
uzf_pkdat = []
vks = 10.0
thtr = 0.05
thts = 0.30
thti = 0.15
eps = 3.5

for k in np.arange(nlay):
    for i in np.arange(0, ncpl, 1):
        if k == 0:
            landflg = 1
            surfdp = 0.25
        else:
            landflg = 0
            surfdp = 1e-6

        if k == nlay - 1:
            ivertcon = -1
        else:
            ivertcon = iuzno + ncpl

        bndnm = "uzf" + f"{int(i + 1):03d}"
        uzf_pkdat.append(
            # iuzno     cellid landflag ivertcn surfdp vks thtr thts thti eps [bndnm]
            [
                iuzno,
                (k, i),
                landflg,
                ivertcon,
                surfdp,
                vks,
                thtr,
                thts,
                thti,
                eps,
                bndnm,
            ]
        )

        iuzno += 1


extdp = 14.0
extwc = 0.055
pet = 0.001
zero = 0.0
uzf_spd = {}
for t in np.arange(0, nper, 1):
    spd = []
    iuzno = 0
    for k in np.arange(nlay):
        for i in np.arange(0, ncpl, 1):
            if k == 0:
                if t == 0:
                    finf = 0.15
                if t == 1:
                    finf = 0.15
                if t == 2:
                    finf = 0.15
                if t == 3:
                    finf = 0.15
                if t == 4:
                    finf = 0.15

            spd.append([iuzno, finf, pet, extdp, extwc, zero, zero, zero])
            iuzno += 1

    uzf_spd.update({t: spd})


# Work up the GHB boundary
ghb_ids = [(ncol - 1) + i * ncol for i in range(nrow)]
ghb_spd = []
cond = 1e4
for k in np.arange(3, 5, 1):
    for i in ghb_ids:
        ghb_spd.append([(k, i), 14.0, cond])


def build_models(idx, test):
    name = cases[idx]

    # build MODFLOW 6 files
    ws = test.workspace
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )

    # time discretization
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=name, newtonoptions="NEWTON", save_flows=True
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="MODERATE",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )
    sim.register_ims_package(ims, [gwf.name])

    # disv
    disv = flopy.mf6.ModflowGwfdisv(gwf, **disvkwargs)

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=True, icelltype=1, k=0.1, k33=1
    )

    # aquifer storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf, iconvert=1, ss=1e-5, sy=0.2, transient=True
    )

    # general-head boundary
    ghb = flopy.mf6.ModflowGwfghb(
        gwf, print_flows=True, stress_period_data=ghb_spd
    )

    # unsaturated-zone flow
    etobs = []
    i = 4
    # Seems as though these are 1-based and not 0-based, like the rest of flopy
    for j in list(np.arange(40, 50, 1)) + list(np.arange(140, 150, 1)):
        etobs.append(("uzet_" + str(j + 1), "uzet", (j,)))
        etobs.append(("uzf-gwet_" + str(j + 1), "uzf-gwet", (j,)))

    uzf_obs = {f"{name}.uzfobs": etobs}

    uzf = flopy.mf6.ModflowGwfuzf(
        gwf,
        print_flows=True,
        save_flows=True,
        simulate_et=True,
        simulate_gwseep=True,
        linear_gwet=True,
        observations=uzf_obs,
        boundnames=True,
        ntrailwaves=15,
        nwavesets=40,
        nuzfcells=len(uzf_pkdat),
        packagedata=uzf_pkdat,
        perioddata=uzf_spd,
        budget_filerecord=f"{name}.uzf.bud",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        filename=f"{name}.oc",
    )

    # Print human-readable heads
    obs_lst = []
    for k in np.arange(0, 1, 1):
        for i in np.arange(40, 50, 1):
            obs_lst.append(["obs_" + str(i + 1), "head", (k, i)])

    obs_dict = {f"{name}.obs.csv": obs_lst}
    obs = flopy.mf6.ModflowUtlobs(
        gwf, pname="head_obs", digits=20, continuous=obs_dict
    )

    return sim, None


def check_output(idx, test):
    # Next, get the binary printed heads
    fpth = os.path.join(test.workspace, test.name + ".hds")
    hobj = flopy.utils.HeadFile(fpth, precision="double")
    hds = hobj.get_alldata()
    hds = hds.reshape((np.sum(nstp), 5, 10, 10))

    # Get the MF6 cell-by-cell fluxes
    bpth = os.path.join(test.workspace, test.name + ".cbc")
    bobj = flopy.utils.CellBudgetFile(bpth, precision="double")
    bobj.get_unique_record_names()
    # '          STO-SS'
    # '          STO-SY'
    # '    FLOW-JA-FACE'
    # '             GHB'
    # '       UZF-GWRCH'
    # '         UZF-GWD'
    # '        UZF-GWET'

    gwet = bobj.get_data(text="UZF-GWET")
    gwet = np.array(gwet)
    gwetl = gwet.ravel().tolist()
    gwetv = np.array([itm[2] for i, itm in enumerate(gwetl)])
    gwet = gwetv.reshape((np.sum(nstp), 5, 10, 10))

    # Also retrieve the binary UZET output
    uzpth = os.path.join(test.workspace, test.name + ".uzf.bud")
    uzobj = flopy.utils.CellBudgetFile(uzpth, precision="double")
    uzobj.get_unique_record_names()
    #  b'    FLOW-JA-FACE',
    #  b'             GWF',
    #  b'    INFILTRATION',
    #  b'         REJ-INF',
    #  b'            UZET',
    #  b'         STORAGE'
    uzet = uzobj.get_data(text="UZET")
    uzet = np.array(uzet)
    uzetl = uzet.ravel().tolist()
    uzetv = np.array([itm[2] for i, itm in enumerate(uzetl)])
    uzet = uzetv.reshape((np.sum(nstp), 5, 10, 10))

    # Confirm that the groundwater gradient dips to the right
    for tm in np.arange(hds.shape[0]):
        arr = hds[tm]
        for ly in np.arange(hds.shape[1]):
            hdlayer = arr[ly]
            for rw in np.arange(arr.shape[0]):
                fullrw = hdlayer[rw]
                assert np.all(
                    np.diff(fullrw) < 0
                ), "GW heads not decreasing to the right"

    # After confirming heads drop off to the right,
    # complete checks that ET totals & character (UZET vs GWET)
    # make sense
    #
    # Need a 4D array boolean array of whether or not the water table is
    # present
    hds_flg = np.zeros_like(hds)
    for t in np.arange(hds.shape[0]):
        for k in np.arange(hds.shape[1]):
            for i in np.arange(hds.shape[2]):
                for j in np.arange(hds.shape[3]):
                    if hds[t, k, i, j] > botm[k]:
                        hds_flg[t, k, i, j] = 1

    # Given that heads dip to the right, the GWET should also drop off to the
    # right
    gwetf = gwet.sum(axis=1)
    for tm in np.arange(gwetf.shape[0]):
        arr = gwetf[tm]
        for rw in np.arange(arr.shape[0]):
            fullrw = arr[rw]
            for cl in np.arange(len(fullrw) - 1):
                assert abs(fullrw[cl]) + 0.01 >= abs(fullrw[cl + 1]), (
                    "gwet not decreasing to the right as expected. Stress Period: "
                    + str(tm + 1)
                    + "; Row: "
                    + str(rw + 1)
                    + "; Col: "
                    + str(cl + 1)
                )

    # Given the spatial uniformity of the model setup, wherever the water
    # table is below the bottom of layer 1, the UZET should be equal
    # (within a very tight tolerance).
    for tm in np.arange(hds_flg.shape[0]):
        flg = False
        for i in np.arange(hds_flg.shape[2]):
            for j in np.arange(hds_flg.shape[3]):
                if hds_flg[tm, 0, i, j] == 0:
                    if not flg:
                        uzet_val2chk = uzet[tm, 0, i, j]
                        flg = not flg
                    else:
                        assert np.isclose(
                            [uzet_val2chk], [uzet[tm, 0, i, j]], atol=1e-6
                        ), "UZET in layer 1 is not uniform, but should be"

    # Also because the groundwater heads dip to the right, that means the
    # UZ thickness increases to the right.  As a result, the summed UZET
    # should increase to the right (the opposite of the GWET)
    uzets = uzet.sum(axis=0)
    for tm in np.arange(uzets.shape[0]):
        arr = uzets[tm]
        for rw in np.arange(arr.shape[0]):
            fullrw = arr[rw]
            for cl in np.arange(len(fullrw) - 1):
                assert abs(fullrw[cl]) <= abs(fullrw[cl + 1]) + 0.01, (
                    "gwet not decreasing to the right as expected. Stress Period: "
                    + str(tm + 1)
                    + "; Row: "
                    + str(rw + 1)
                    + "; Col: "
                    + str(cl + 1)
                    + f"{fullrw[cl]} should be less than or equal to {abs(fullrw[cl + 1])}"
                )

    # Confirm that total simulated ET does not exceed potential ET.
    # Sum UZET and GWET
    total_et = abs(uzet) + abs(gwet)
    # Sum ET from across layers
    total_et = total_et.sum(axis=1)
    for tm in np.arange(total_et.shape[0]):
        for rw in np.arange(total_et.shape[1]):
            for cl in np.arange(total_et.shape[2]):
                assert total_et[tm, rw, cl] <= pet, (
                    "simulated ET exceeds user-specified potential ET.  Stress Period: "
                    + str(tm + 1)
                    + "; Row: "
                    + str(rw + 1)
                    + "; Col: "
                    + str(cl + 1)
                )

    print("Finished running checks")


@pytest.mark.slow
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
