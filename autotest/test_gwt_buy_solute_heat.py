import os
import sys

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

ex = ["gwtbuy"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))


def build_model(idx, dir):

    lx = 2000.0
    lz = 1000.0

    nlay = 100
    nrow = 1
    ncol = 200
    nper = 1
    delr = lx / ncol
    delc = 1.0
    top = 1.0
    delz = lz / nlay
    botm = list(top - np.arange(delz, nlay * delz + delz, delz))

    perlen = [200000]
    nstp = [10]
    tsmult = [1.0]
    steady = [True]
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-10, 1e-6, 0.97

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
    gwfname = "flow"
    gwtsname = "salinity"
    gwthname = "temperature"

    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname)

    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
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
        filename=f"{gwfname}.ims",
    )
    sim.register_ims_package(imsgwf, [gwfname])

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
    ic = flopy.mf6.ModflowGwfic(gwf, strt=1000.0)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=0,
        k=10.0,
        k33=0.1,
    )

    # storage
    sto = flopy.mf6.ModflowGwfsto(gwf, ss=1.0e-5, iconvert=0)

    pd = [
        (0, 0.7, 0.0, gwtsname, "SALINITY"),
        (1, -0.375, 25.0, gwthname, "TEMPERATURE"),
    ]
    fname = f"{gwfname}.buy.bin"
    buy = flopy.mf6.ModflowGwfbuy(
        gwf, density_filerecord=fname, nrhospecies=len(pd), packagedata=pd
    )

    wellist1 = []
    qwell = 10 / nlay
    conc_inflow = 0.0
    temp_inflow = 25.0
    for k in range(nlay):
        wellist1.append([(k, 0, 0), qwell, conc_inflow, temp_inflow])
    wel1 = flopy.mf6.ModflowGwfwel(
        gwf,
        stress_period_data=wellist1,
        print_input=True,
        print_flows=True,
        save_flows=False,
        pname="WEL-1",
        auxiliary=["SALINITY", "TEMPERATURE"],
    )

    # ghb files
    ghb_value = 1000.0
    ghb_cond = 10.0 * (1.0 * 10.0) / 5.0
    ghb_salinity = 35.0
    ghb_temperature = 5.0
    ghb_density = (
        1000.0 + 0.7 * ghb_salinity - 0.375 * (ghb_temperature - 25.0)
    )
    ghblist1 = []
    for k in range(nlay):
        ghblist1.append(
            [
                (k, 0, ncol - 1),
                ghb_value,
                ghb_cond,
                ghb_salinity,
                ghb_temperature,
                ghb_density,
            ]
        )
    ghb1 = flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=ghblist1,
        print_input=True,
        print_flows=True,
        save_flows=False,
        pname="GHB-1",
        auxiliary=["SALINITY", "TEMPERATURE", "DENSITY"],
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # create gwt model
    salinity_transport = True
    if salinity_transport:
        gwts = flopy.mf6.ModflowGwt(sim, modelname=gwtsname)

        imsgwt = flopy.mf6.ModflowIms(
            sim,
            print_option="ALL",
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
            filename=f"{gwtsname}.ims",
        )
        sim.register_ims_package(imsgwt, [gwts.name])

        dis = flopy.mf6.ModflowGwtdis(
            gwts,
            nlay=nlay,
            nrow=nrow,
            ncol=ncol,
            delr=delr,
            delc=delc,
            top=top,
            botm=botm,
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwtic(gwts, strt=35.0)

        # advection
        adv = flopy.mf6.ModflowGwtadv(gwts, scheme="UPSTREAM")

        # dispersion
        dsp = flopy.mf6.ModflowGwtdsp(gwts, alh=10.0, ath1=0.1, diffc=1.0e-10)

        # mass storage and transfer
        porosity = 0.35
        mst = flopy.mf6.ModflowGwtmst(gwts, porosity=porosity)

        # sources
        sourcerecarray = [
            ("GHB-1", "AUX", "SALINITY"),
            ("WEL-1", "AUX", "SALINITY"),
        ]
        ssm = flopy.mf6.ModflowGwtssm(gwts, sources=sourcerecarray)

        # output control
        oc = flopy.mf6.ModflowGwtoc(
            gwts,
            budget_filerecord=f"{gwtsname}.cbc",
            concentration_filerecord=f"{gwtsname}.ucn",
            concentrationprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("CONCENTRATION", "ALL")],
            printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
        )

        # GWF GWT exchange
        gwfgwts = flopy.mf6.ModflowGwfgwt(
            sim,
            exgtype="GWF6-GWT6",
            exgmnamea=gwfname,
            exgmnameb=gwtsname,
            filename=f"{name}-s.gwfgwt",
        )

    # create gwt model
    heat_transport = True
    if heat_transport:
        gwth = flopy.mf6.ModflowGwt(sim, modelname=gwthname)

        imsgwt = flopy.mf6.ModflowIms(
            sim,
            print_option="ALL",
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
            filename=f"{gwthname}.ims",
        )
        sim.register_ims_package(imsgwt, [gwth.name])

        dis = flopy.mf6.ModflowGwtdis(
            gwth,
            nlay=nlay,
            nrow=nrow,
            ncol=ncol,
            delr=delr,
            delc=delc,
            top=top,
            botm=botm,
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwtic(gwth, strt=5.0)

        # advection
        adv = flopy.mf6.ModflowGwtadv(gwth, scheme="UPSTREAM")

        # dispersion
        dsp = flopy.mf6.ModflowGwtdsp(gwth, xt3d_off=True, diffc=0.150309621)

        # mass storage and transfer
        porosity = 0.35
        mst = flopy.mf6.ModflowGwtmst(
            gwth, porosity=porosity, bulk_density=1761.5, distcoef=2.0e-4
        )

        # sources
        sourcerecarray = [
            ("GHB-1", "AUX", "TEMPERATURE"),
            ("WEL-1", "AUX", "TEMPERATURE"),
        ]
        ssm = flopy.mf6.ModflowGwtssm(gwth, sources=sourcerecarray)

        cnclist = []
        for k in range(nlay):
            cnclist.append([(k, 0, ncol - 1), 5.0])
        cnc = flopy.mf6.ModflowGwtcnc(
            gwth,
            maxbound=len(cnclist),
            stress_period_data=cnclist,
            save_flows=False,
            pname="CNC-1",
        )

        # output control
        oc = flopy.mf6.ModflowGwtoc(
            gwth,
            budget_filerecord=f"{gwthname}.cbc",
            concentration_filerecord=f"{gwthname}.ucn",
            concentrationprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("CONCENTRATION", "ALL")],
            printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
        )

        # GWF GWT exchange
        gwfgwth = flopy.mf6.ModflowGwfgwt(
            sim,
            exgtype="GWF6-GWT6",
            exgmnamea=gwfname,
            exgmnameb=gwthname,
            filename=f"{name}-h.gwfgwt",
        )

    return sim, None


def make_plot(sim):
    print("making plots...")
    name = ex[sim.idxsim]
    ws = exdirs[sim.idxsim]
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws)
    gwfname = "flow"
    gwtsname = "salinity"
    gwthname = "temperature"
    gwf = sim.get_model(gwfname)
    gwts = sim.get_model(gwtsname)
    gwth = sim.get_model(gwthname)

    fname = gwtsname + ".ucn"
    fname = os.path.join(ws, fname)
    cobj = flopy.utils.HeadFile(
        fname, text="CONCENTRATION"
    )  # , precision='double')
    conc = cobj.get_alldata()

    fname = gwthname + ".ucn"
    fname = os.path.join(ws, fname)
    tobj = flopy.utils.HeadFile(
        fname, text="CONCENTRATION"
    )  # , precision='double')
    temperature = tobj.get_alldata()

    fname = gwfname + ".buy.bin"
    fname = os.path.join(ws, fname)
    dobj = flopy.utils.HeadFile(fname, text="DENSITY")  # , precision='double')
    dense = dobj.get_alldata()

    idxtime = -1

    import matplotlib.pyplot as plt

    fig = plt.figure(figsize=(10, 10))
    nplotrows = 3
    ax = fig.add_subplot(nplotrows, 1, 1, aspect="equal")
    pxs = flopy.plot.PlotCrossSection(model=gwf, ax=ax, line={"row": 0})
    # pxs.plot_grid()
    pxs.plot_bc(ftype="WEL")
    pxs.plot_bc(ftype="GHB")
    a = conc[idxtime]
    pa = pxs.plot_array(a, cmap="jet", alpha=0.25)
    cs = pxs.contour_array(
        a, levels=35.0 * np.array([0.01, 0.5, 0.99]), colors="y"
    )
    plt.colorbar(pa, shrink=0.5)
    ax.set_title("SALINITY")

    ax = fig.add_subplot(nplotrows, 1, 2, aspect="equal")
    pxs = flopy.plot.PlotCrossSection(model=gwf, ax=ax, line={"row": 0})
    # pxs.plot_grid()
    pxs.plot_bc(ftype="WEL")
    pxs.plot_bc(ftype="GHB")
    a = temperature[idxtime]
    pa = pxs.plot_array(a, cmap="jet", alpha=0.25)
    cs = pxs.contour_array(
        a, levels=5 + 20.0 * np.array([0.01, 0.5, 0.99]), colors="y"
    )
    plt.colorbar(pa, shrink=0.5)
    ax.set_title("TEMPERATURE")

    ax = fig.add_subplot(nplotrows, 1, 3, aspect="equal")
    pxs = flopy.plot.PlotCrossSection(model=gwf, ax=ax, line={"row": 0})
    # pxs.plot_grid()
    pxs.plot_bc(ftype="WEL")
    pxs.plot_bc(ftype="GHB")
    a = dense[idxtime]
    pa = pxs.plot_array(a, cmap="jet", alpha=0.25)
    # cs = pxs.contour_array(a, levels=5+20.*np.array([0.01, .5, 0.99]),
    #                       colors='y')
    plt.colorbar(pa, shrink=0.5)
    ax.set_title("DENSITY")

    plt.draw()
    fname = os.path.join(ws, gwtsname + ".png")
    plt.savefig(fname)

    return


def eval_transport(sim):
    print("evaluating transport...")

    makeplot = False
    if makeplot:
        make_plot(sim)

    name = ex[sim.idxsim]
    ws = sim.simpath
    gwfname = "flow"
    gwtsname = "salinity"
    gwthname = "temperature"

    fname = gwfname + ".buy.bin"
    fname = os.path.join(ws, fname)
    dobj = flopy.utils.HeadFile(fname, text="DENSITY")  # , precision='double')
    dense = dobj.get_alldata()

    fname = gwtsname + ".ucn"
    fname = os.path.join(ws, fname)
    cobj = flopy.utils.HeadFile(
        fname, text="CONCENTRATION"
    )  # , precision='double')
    conc = cobj.get_alldata()

    fname = gwthname + ".ucn"
    fname = os.path.join(ws, fname)
    tobj = flopy.utils.HeadFile(
        fname, text="CONCENTRATION"
    )  # , precision='double')
    temperature = tobj.get_alldata()

    # density is lagged, so use c and t from previous timestep
    c = conc[-2]
    t = temperature[-2]
    d = dense[-1]
    densecalculated = 1000.0 + 0.7 * c - 0.375 * (t - 25.0)

    if not np.allclose(d, densecalculated):

        print("density is not correct")
        fname = os.path.join(ws, "a-dense.txt")
        np.savetxt(fname, d.reshape(200, 100))
        fname = os.path.join(ws, "a-conc.txt")
        np.savetxt(fname, c.reshape(200, 100))
        fname = os.path.join(ws, "a-temperature.txt")
        np.savetxt(fname, t.reshape(200, 100))
        fname = os.path.join(ws, "a-densecalculated.txt")
        np.savetxt(fname, densecalculated.reshape(200, 100))
        assert False, "density is not correct"

    # assert False

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir, exfunc=eval_transport, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
