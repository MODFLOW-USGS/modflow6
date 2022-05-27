"""
MODFLOW 6 Autotest
A test of DISV with UZF.  Originally created due to a possible bug in the 
ASCII output file generated by UZF.  Uses quadrilateral cells.  The cells 
are created from a numpy grid with cells that are 1m x 1m.  Althought a DISV
grid, arrangement mimics that of a 10 row x 10 col x 5 layer DIS grid.  
Inflow from infiltration on the top of the grid, flow exits via GHB 
boundary on the right-hand side of the model.
"""

import os
import pytest
import sys
import numpy as np

try:
    import flopy
    import flopy.utils.cvfdutil
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)


from framework import testing_framework
from simulation import Simulation


ex = ["disv_with_uzf"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

nlay = 5
nper = 5
perlen = [10] * 5
nstp = [10] * 5
tsmult = len(perlen) * [1.0]
botm = [20.0, 15.0, 10.0, 5.0, 0.0]
strt = 20

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97

ghb_ids = []


def create_disv_mesh():
    # Create a grid of verts
    nx, ny = (11, 11)
    x = np.linspace(0, 10, nx)
    y = np.linspace(0, 10, ny)
    xv, yv = np.meshgrid(x, y)
    yv = np.flipud(yv)

    verts = []
    vid = 0
    vert_lkup = {}
    for i in yv[:, 0]:
        for j in xv[0, :]:
            vert_lkup.update({(float(j), float(i)): vid})
            verts.append([int(vid), float(j), float(i)])
            vid += 1

    ivert = []
    ivid = 0
    xyverts = []
    xc, yc = [], []  # for storing the cell center location
    for i in yv[:-1, 0]:
        for j in xv[0, :-1]:
            xlst, ylst = [], []
            vid_lst = []
            # Start with upper-left corner and go clockwise
            for ct in [0, 1, 2, 3]:
                if ct == 0:
                    iadj = 0.0
                    jadj = 0.0
                elif ct == 1:
                    iadj = 0.0
                    jadj = 1.0
                elif ct == 2:
                    iadj = -1.0
                    jadj = 1.0
                elif ct == 3:
                    iadj = -1.0
                    jadj = 0.0

                vid = vert_lkup[(float(j + jadj), float(i + iadj))]
                vid_lst.append(vid)

                xlst.append(float(j + jadj))
                ylst.append(float(i + iadj))

            xc.append(np.mean(xlst))
            yc.append(np.mean(ylst))
            xyverts.append(list(zip(xlst, ylst)))

            rec = [ivid] + vid_lst
            ivert.append(rec)

            # if ivert part of right boundary, store id
            if j == 9.0:
                ghb_ids.append(ivid)

            ivid += 1

    # finally, create a cell2d record
    cell2d = []
    for ix, iv in enumerate(ivert):
        xvt, yvt = np.array(xyverts[ix]).T
        if flopy.utils.geometry.is_clockwise(xvt, yvt):
            rec = [iv[0], xc[ix], yc[ix], len(iv[1:])] + iv[1:]
        else:
            iiv = iv[1:][::-1]
            rec = [iv[0], xc[ix], yc[ix], len(iiv)] + iiv

        cell2d.append(rec)

    return verts, cell2d


verts, cell2d = create_disv_mesh()

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
    for i in np.arange(0, len(cell2d), 1):
        if k == 0:
            landflg = 1
            surfdp = 0.25
        else:
            landflg = 0
            surfdp = 1e-6

        if k == nlay - 1:
            ivertcon = -1
        else:
            ivertcon = iuzno + len(cell2d)

        bndnm = "uzf" + "{0:03d}".format(int(i + 1))
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
        for i in np.arange(0, len(cell2d), 1):
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
ghb_spd = []
cond = 1e4
for k in np.arange(3, 5, 1):
    for i in ghb_ids:
        ghb_spd.append([(k, i), 14.0, cond])


def build_model(idx, dir):

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
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

    ncpl = len(cell2d)
    nvert = len(verts)
    disv = flopy.mf6.ModflowGwfdisv(
        gwf,
        nlay=nlay,
        ncpl=ncpl,
        nvert=nvert,
        top=25.0,
        botm=botm,
        vertices=verts,
        cell2d=cell2d,
    )

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

    uzf_obs = {"{}.uzfobs".format(name): etobs}

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
        budget_filerecord="{}.uzf.bud".format(name),
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


def eval_model(sim):
    print("evaluating model...")

    idx = sim.idxsim
    name = ex[idx]
    ws = os.path.join("temp", name)

    # Next, get the binary printed heads
    fpth = os.path.join(ws, name + ".hds")
    hobj = flopy.utils.HeadFile(fpth, precision="double")
    hds = hobj.get_alldata()
    hds = hds.reshape((50, 5, 10, 10))

    # Get the MF6 cell-by-cell fluxes
    bpth = os.path.join(ws, name + ".cbc")
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
    gwet = gwetv.reshape((50, 5, 10, 10))

    # Also retrieve the binary UZET output
    uzpth = os.path.join(ws, name + ".uzf.bud")
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
    uzet = uzetv.reshape((50, 5, 10, 10))

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
                assert abs(fullrw[cl]) >= abs(fullrw[cl + 1]), (
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
                assert abs(fullrw[cl]) <= abs(fullrw[cl + 1]), (
                    "gwet not decreasing to the right as expected. Stress Period: "
                    + str(tm + 1)
                    + "; Row: "
                    + str(rw + 1)
                    + "; Col: "
                    + str(cl + 1)
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
    test.run_mf6(Simulation(dir, exfunc=eval_model, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_model, idxsim=idx)
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
