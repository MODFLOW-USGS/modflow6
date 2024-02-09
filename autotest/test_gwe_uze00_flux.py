#  - Similar to test_gwe_uze00.py; however, this looks into whether the
#    flux input is correct without pinning the temperature of the top-most
#    cell to a specified value.
#
#  - Outer columns not active for unsaturated zone, but are present to host
#    constant head boundaries at the bottom of the model.
#
#     +-------+
#     |///////| = Inactive cell
#     +-------+
#
#     Model depiction:
#
#     +-------+-------+-------+
#     |///////|       |///////|  Layer 1
#     +-------+-------+-------+
#     |///////|       |///////|  Layer 2
#     +-------+-------+-------+
#     |///////|       |///////|  Layer 3
#     +-------+-------+-------+
#     |///////|       |///////|
#     + -- -- + -- -- + -- -- +
#     |///////|       |///////|  Layer x  (Middle portion of model not shown)
#     + -- -- + -- -- + -- -- +
#     |///////|       |///////|
#     +-------+-------+-------+
#     |       |       |       |  Layer 99
#     +-------+-------+-------+
#     |       |       |       |  Layer 100
#     +-------+-------+-------+

import os

import flopy
import numpy as np
import pytest
import math

import flopy.utils.binaryfile as bf
from framework import TestFramework


# Analytical solution derived by Alden, similar in form to
# Barends (2010) Equation 5 - but remember that that solution
# pins the temperature of the top cell to a specified temperature
def flux_analyt(t, z, qt0, qtinfil, v, d):
    if t == 0.0:
        flux = qt0
    else:
        denom = 2.0 * math.sqrt(d * t)
        ztermm = (z - v * t) / denom
        ztermp = (z + v * t) / denom
        vterm = v * z / d
        if vterm < 100.0:
            # might need to adjust this limit
            flux = qt0 + (qtinfil - qt0) * 0.5 * (
                math.erfc(ztermm) + math.exp(vterm) * math.erfc(ztermp)
            )
        else:
            zeta = 1.0 / (1.0 + 0.47047 * ztermp)
            polyterm = zeta * (
                0.3480242 + zeta * (-0.0958798 + zeta * 0.7478556)
            )
            flux = qt0 + 0.5 * (qtinfil - qt0) * (
                math.erfc(ztermm) + math.exp(vterm - ztermp ** 2) * polyterm
            )
    return flux


def temp_analyt(t, z, t0, tinfil, v, d):
    if t == 0.0:
        temp = t0
    else:
        denom = 2.0 * math.sqrt(d * t)
        ztermm = (z - v * t) / denom
        ztermp = (z + v * t) / denom
        vterm = v * z / d
        if vterm < 100.0:
            # might need to adjust this limit
            temp = t0 + 0.5 * (tinfil - t0) * (
                math.erfc(ztermm) + math.exp(vterm) * math.erfc(ztermp)
            )
        else:
            zeta = 1.0 / (1.0 + 0.47047 * ztermp)
            polyterm = zeta * (
                0.3480242 + zeta * (-0.0958798 + zeta * 0.7478556)
            )
            temp = t0 + 0.5 * (tinfil - t0) * (
                math.erfc(ztermm) + math.exp(vterm - ztermp ** 2) * polyterm
            )
    return temp


# Model units
length_units = "meters"
time_units = "days"

nlay, nrow, ncol = 101, 1, 3
nper = 2
perlen = [1.0e9, 100.0]
nstp = [1, 100]
tsmult = len(perlen) * [1.0]

delr = 1.0
delc = 1.0
delz = 0.1  # 10 cm
strt = 0.05
top = 10.0005
botm = [
    9.9995
]  # Top layer is very thin for application of the boundary condition
for i in np.arange(1, nlay):
    bot = 10.0 - (i * delz)
    botm.append(round(bot, 1))

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97
steady = {0: False, 1: False}
transient = {0: True, 1: True}

idomain_u = [0, 1, 0]
idomain_l = [1, 1, 1]
idomain = []
for i in np.arange(nlay):
    if i < 99:
        idomain.append(idomain_u)
    else:
        idomain.append(idomain_l)

idomain = np.array(idomain)

strt_temp = 10.0
scheme = "UPSTREAM"
dispersivity = 0.0
prsity = 0.2

# transient uzf info
# iuzno  cellid landflg ivertcn surfdp vks thtr thts thti eps [bndnm]
uzf_pkdat = [[0, (0, 0, 1), 1, 1, 0.00001, 1, 0.0001, 0.20, 0.055, 4]]

# Continue building the UZF list of objects
for iuzno in np.arange(1, 101, 1):
    if iuzno < nlay - 1:
        ivertconn = iuzno + 1
    else:
        ivertconn = -1

    uzf_pkdat.append(
        [iuzno, (iuzno, 0, 1), 0, ivertconn, 0.01, 1, 0.0001, 0.20, 0.055, 4]
    )

iuz_cell_dict = {}
cell_iuz_dict = {}
for i, itm in enumerate(uzf_pkdat):
    iuz_cell_dict.update({itm[0]: (itm[1][0], itm[1][1], itm[1][2])})
    cell_iuz_dict.update({(itm[1][0], itm[1][1], itm[1][2]): itm[0]})

finf = 0.01
extdp = 0.0
pet = 0.0
extwc = 0.0
zero = 0.0
uzf_spd = {
    0: [[0, finf, pet, extdp, extwc, zero, zero, zero]],
    1: [[0, finf, pet, extdp, extwc, zero, zero, zero]],
}

cases = ["uze00_flux"]


def build_models(idx, test):
    name = cases[idx]

    # build MODFLOW 6 files
    ws = test.workspace
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )

    # create tdis package
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    flopy.mf6.ModflowTdis(
        sim, time_units=time_units, nper=nper, perioddata=tdis_rc
    )

    gwfname = "gwf_" + name
    gwename = "gwe_" + name

    newtonoptions = ["NEWTON", "UNDER_RELAXATION"]
    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        newtonoptions=newtonoptions,
        save_flows=True,
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
        filename=f"{gwfname}.ims",
    )
    sim.register_ims_package(ims, [gwf.name])

    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
        filename=f"{gwfname}.dis",
    )

    # initial conditions
    flopy.mf6.ModflowGwfic(
        gwf,
        strt=strt,
        filename=f"{gwfname}.ic",
    )

    # node property flow
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=True,
        icelltype=1,
        k=100.0,
        k33=10,
        filename=f"{gwfname}.npf",
    )

    # aquifer storage
    flopy.mf6.ModflowGwfsto(
        gwf,
        iconvert=1,
        ss=1e-5,
        sy=prsity,
        steady_state=steady,
        transient=transient,
        filename=f"{gwfname}.sto",
    )

    # chd files
    chdval = 0.05
    chdspd = {0: [[(100, 0, 0), chdval, 10.0], [(100, 0, 2), chdval, 10.0]]}

    flopy.mf6.ModflowGwfchd(
        gwf,
        auxiliary=["TEMPERATURE"],
        print_flows=True,
        stress_period_data=chdspd,
        pname="CHD-1",
        filename=f"{gwfname}.chd",
    )

    # Unsaturated-zone flow package
    flopy.mf6.ModflowGwfuzf(
        gwf,
        print_flows=True,
        save_flows=True,
        wc_filerecord=gwfname + ".uzfwc.bin",
        simulate_et=False,
        simulate_gwseep=False,
        linear_gwet=False,
        boundnames=False,
        ntrailwaves=15,
        nwavesets=40,
        nuzfcells=len(uzf_pkdat),
        packagedata=uzf_pkdat,
        perioddata=uzf_spd,
        budget_filerecord=f"{gwfname}.uzf.bud",
        pname="UZF-1",
        filename=f"{gwfname}.uzf",
    )

    # output control
    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        filename=f"{gwfname}.oc",
    )

    # ----------------------------------
    # Instantiating MODFLOW 6 GWE model
    # ----------------------------------
    gwe = flopy.mf6.ModflowGwe(
        sim, modelname=gwename, model_nam_file=f"{gwename}.nam"
    )
    gwe.name_file.save_flows = True

    imsgwe = flopy.mf6.ModflowIms(
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
        filename="{}.ims".format(gwename),
    )
    sim.register_ims_package(imsgwe, [gwe.name])

    # Instantiating MODFLOW 6 transport discretization package
    flopy.mf6.ModflowGwedis(
        gwe,
        nogrb=True,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
        pname="DIS",
        filename=f"{gwename}.dis",
    )

    # Instantiating MODFLOW 6 transport initial concentrations
    flopy.mf6.ModflowGweic(
        gwe,
        strt=strt_temp,
        pname="IC",
        filename=f"{gwename}.ic",
    )

    # Instantiating MODFLOW 6 transport advection package
    flopy.mf6.ModflowGweadv(
        gwe, scheme=scheme, pname="ADV", filename="{}.adv".format(gwename)
    )

    # Instantiating MODFLOW 6 transport dispersion package
    flopy.mf6.ModflowGwecnd(
        gwe,
        xt3d_off=False,
        alh=dispersivity,
        ath1=dispersivity,
        ktw=0.5918 * 86400,
        kts=0.2700 * 86400,
        pname="DSP",
        filename=f"{gwename}.dsp",
    )

    # Instantiating MODFLOW 6 transport mass storage package
    rhow = 1000.0
    cpw = 4183.0
    lhv = 2500.0
    flopy.mf6.ModflowGweest(
        gwe,
        save_flows=True,
        porosity=prsity,
        cps=760.0,
        rhos=1500.0,
        packagedata=[cpw, rhow, lhv],
        pname="MST",
        filename=f"{gwename}.mst",
    )

    # Instantiating MODFLOW 6 transport source-sink mixing package
    srctype = "AUX"
    auxname = "TEMPERATURE"
    pname = ["CHD-1"]
    # Inpput to SSM is: <pname> <srctype> <auxname>
    sources = [[itm, srctype, auxname] for itm in pname]

    flopy.mf6.ModflowGwessm(
        gwe,
        sources=sources,
        pname="SSM",
        filename=f"{gwename}.ssm",
    )

    # Instantiating MODFLOW 6 energy transport source-sink mixing package
    uzepackagedata = [(iuz, 10.0) for iuz in range(nlay)]
    uzeperioddata = {
        0: [[0, "INFILTRATION", 10.0]],
        1: [[0, "INFILTRATION", 20.0]],
    }

    flopy.mf6.ModflowGweuze(
        gwe,
        flow_package_name="UZF-1",
        boundnames=False,
        save_flows=True,
        print_input=True,
        print_flows=True,
        print_temperature=True,
        temperature_filerecord=gwename + ".uze.bin",
        budget_filerecord=gwename + ".uze.bud",
        packagedata=uzepackagedata,
        uzeperioddata=uzeperioddata,
        pname="UZE-1",
        filename=f"{gwename}.uze",
    )

    # Instantiate MODFLOW 6 heat transport output control package
    flopy.mf6.ModflowGweoc(
        gwe,
        pname="OC",
        budget_filerecord="{}.cbc".format(gwename),
        temperature_filerecord="{}.ucn".format(gwename),
        temperatureprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("TEMPERATURE", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("TEMPERATURE", "ALL"), ("BUDGET", "ALL")],
        filename=f"{gwename}.oc",
    )

    # Instantiate Gwf-Gwe Exchange package
    flopy.mf6.ModflowGwfgwe(
        sim,
        exgtype="GWF6-GWE6",
        exgmnamea=gwfname,
        exgmnameb=gwename,
        filename="{}.gwfgwe".format(gwename),
    )

    return sim, None


def check_output(idx, test):
    print("evaluating flow...")

    name = cases[idx]
    gwfname = "gwf_" + name
    gwename = "gwe_" + name
    ws = test.workspace

    # check some output...
    wc_fl = gwfname + ".uzfwc.bin"
    wcobj = flopy.utils.HeadFile(os.path.join(ws, wc_fl), text="water-content")
    wc = wcobj.get_alldata()

    # temperature output
    fl2 = gwename + ".uze.bin"
    uzeobj = flopy.utils.HeadFile(os.path.join(ws, fl2), text="TEMPERATURE")
    temps = uzeobj.get_alldata()

    # Cell flows output
    qfile = gwename + ".cbc"
    gweflowsobj = flopy.utils.CellBudgetFile(os.path.join(ws, qfile))

    # Binary grid file needed for post-processing
    fgrb = gwfname + ".dis.grb"
    grb_file = os.path.join(ws, fgrb)

    # UZE flows
    fuzebud = gwename + ".uze.bud"
    uzeflowsobj = flopy.utils.CellBudgetFile(os.path.join(ws, fuzebud))
    flowsadv = uzeflowsobj.get_data(text="FLOW-JA-FACE")

    t = np.linspace(0.0, 100.0, 101)
    z = np.linspace(0.0, 9.9, 99)

    q = finf  # infiltration rate
    area = delr * delc
    rhos = 1500.0
    Cps = 760.0
    rhow = 1000.0
    Cpw = 4183.0
    rhowCpw = Cpw * rhow
    rhosCps = Cps * rhos

    Kts = 23328.0
    Ktw = 0.0

    steady_wc = wc[1, 0, 0, 1]
    Sw = steady_wc / prsity

    rhoCp_bulk = Sw * prsity * rhowCpw + (1 - prsity) * rhosCps
    Kt_bulk = Sw * prsity * Ktw + (1 - prsity) * Kts
    v = rhowCpw / rhoCp_bulk * q
    D = Kt_bulk / rhoCp_bulk

    t0 = 10.0
    tinfil = 20.0
    qt0 = q * t0
    qtinfil = q * tinfil

    # for converting from J/day to W
    unitadj = 1 / 86400

    # Get analytical solution
    conv10 = []
    cond10 = []
    conv50 = []
    cond50 = []
    conv100 = []
    cond100 = []
    analytical_sln = np.zeros((len(t), len(z)))
    simulated_sln = np.zeros((len(t), len(z)))
    for i, tm in enumerate(t):
        if i == 0:
            gweflowjaface = gweflowsobj.get_data(
                text="FLOW-JA-FACE", kstpkper=(0, 0)
            )
        else:
            gweflowjaface = gweflowsobj.get_data(
                text="FLOW-JA-FACE", kstpkper=(i - 1, 1)
            )
        flowscond = flopy.mf6.utils.postprocessing.get_structured_faceflows(
            gweflowjaface[0][0], grb_file=grb_file
        )
        for j, depth in enumerate(z):
            fluxa = flux_analyt(tm, depth, qt0, qtinfil, v, D)
            analytical_sln[i, j] = fluxa * rhowCpw * unitadj

            (uze1, uze2, floadv) = flowsadv[i][2 * j + 1]
            (fjunk1, flocond, fjunk2) = flowscond[1][j][0]
            flo = floadv * rhowCpw * unitadj + flocond * rhowCpw * unitadj
            if i == 10:
                conv10.append(floadv * rhowCpw * unitadj)
                cond10.append(flocond * rhowCpw * unitadj)
            elif i == 50:
                conv50.append(floadv * rhowCpw * unitadj)
                cond50.append(flocond * rhowCpw * unitadj)
            elif i == 100:
                conv100.append(floadv * rhowCpw * unitadj)
                cond100.append(flocond * rhowCpw * unitadj)

            flux = flo / area
            simulated_sln[i, j] = flux

    # Run checks
    msg0 = (
        "Simulated solution has deviated too far from the analytical solution"
    )
    # Following values are calculated as "percent differences" when determining if fits are acceptable
    # Day 10
    assert (
        np.max(
            ((analytical_sln[10] * rhowCpw) - simulated_sln[10])
            / (analytical_sln[10] * rhowCpw)
            * 100
        )
        <= 0.51091366512
    ), msg0
    assert (
        np.min(
            ((analytical_sln[10] * rhowCpw) - simulated_sln[10])
            / (analytical_sln[10] * rhowCpw)
            * 100
        )
        >= -2.62119104308
    ), msg0

    # Day 50
    assert (
        np.max(
            ((analytical_sln[50] * rhowCpw) - simulated_sln[50])
            / (analytical_sln[50] * rhowCpw)
            * 100
        )
        <= 0.3171034702
    ), msg0
    assert (
        np.min(
            ((analytical_sln[50] * rhowCpw) - simulated_sln[50])
            / (analytical_sln[50] * rhowCpw)
            * 100
        )
        >= -2.52020856408
    ), msg0

    # Day 100
    assert (
        np.max(
            ((analytical_sln[100] * rhowCpw) - simulated_sln[100])
            / (analytical_sln[100] * rhowCpw)
            * 100
        )
        <= 0.37655443171
    ), msg0
    assert (
        np.min(
            ((analytical_sln[100] * rhowCpw) - simulated_sln[100])
            / (analytical_sln[100] * rhowCpw)
            * 100
        )
        >= -2.56004275226
    ), msg0

    # If plot is needed, change next statement to "if True:"
    if False:
        import matplotlib.pyplot as plt
        from matplotlib import transforms
        from matplotlib.collections import PathCollection
        from matplotlib.patches import Patch
        from matplotlib.lines import Line2D

        fig, (ax1, ax2, ax3) = plt.subplots(ncols=3, figsize=(10, 5))
        # 10 days
        # -------
        polys1 = ax1.stackplot(
            z,
            conv10,
            cond10,
            labels=["Convection", "Conduction"],
            colors=["lightseagreen", "lightgreen"],
        )
        ax1.set_xlim((-0.05, 10.05))
        xlims = ax1.get_xlim()
        for poly in polys1:
            for path in poly.get_paths():
                path.vertices = path.vertices[:, ::-1]
        ax1.set_xlim(0.095 * rhowCpw * unitadj, 0.205 * rhowCpw * unitadj)
        ax1.set_ylim(xlims[::-1])
        ax1.plot(simulated_sln[10], z, "-", color="blue", linewidth=1)
        ax1.plot(analytical_sln[10], z, "-.", color="red")
        ax1.text(4.9, 0.4, "10 Days")

        legend_elements = [
            Line2D(
                [0],
                [0],
                linestyle="-",
                color="blue",
                lw=1,
                label="MODFLOW 6 Total Heat Flux",
            ),
            Line2D(
                [0], [0], linestyle="-.", color="red", lw=1, label="Analytical"
            ),
            Patch(
                facecolor="lightseagreen",
                edgecolor="lightseagreen",
                label="Convection",
            ),
            Patch(
                facecolor="lightgreen",
                edgecolor="lightgreen",
                label="Conduction",
            ),
        ]

        ax1.legend(handles=legend_elements, loc="lower right", frameon=False)
        ax1.set_xlabel("Energy Flux, $\dfrac{Watts}{m^2}$")
        ax1.set_ylabel("Depth, m")

        # 50 days
        # -------
        polys2 = ax2.stackplot(
            z,
            conv50,
            cond50,
            labels=["Convection", "Conduction"],
            colors=["lightseagreen", "lightgreen"],
        )
        ax2.set_xlim((-0.05, 10.05))
        xlims = ax2.get_xlim()
        for poly in polys2:
            for path in poly.get_paths():
                path.vertices = path.vertices[:, ::-1]
        ax2.set_xlim(0.095 * rhowCpw * unitadj, 0.205 * rhowCpw * unitadj)
        ax2.set_ylim(xlims[::-1])
        ax2.plot(simulated_sln[50], z, "-", color="blue", linewidth=1)
        ax2.plot(analytical_sln[50], z, "-.", color="red")
        ax2.set_xlabel("Energy Flux, $\dfrac{Watts}{m^2}$")
        ax2.text(4.9, 0.4, "50 Days")

        # 100 days
        # -------
        polys3 = ax3.stackplot(
            z,
            conv100,
            cond100,
            labels=["Convection", "Conduction"],
            colors=["lightseagreen", "lightgreen"],
        )
        ax3.set_xlim((-0.05, 10.05))
        xlims = ax3.get_xlim()
        for poly in polys3:
            for path in poly.get_paths():
                path.vertices = path.vertices[:, ::-1]
        ax3.set_xlim(0.095 * rhowCpw * unitadj, 0.205 * rhowCpw * unitadj)
        ax3.set_ylim(xlims[::-1])
        ax3.plot(simulated_sln[100], z, "-", color="blue", linewidth=1)
        ax3.plot(analytical_sln[100], z, "-.", color="red")
        ax3.set_xlabel("Energy Flux, $\dfrac{Watts}{m^2}$")
        ax3.text(4.9, 0.4, "100 Days")

        plt.tight_layout()
        plt.savefig(os.path.join(ws, "dual_view.png"), format="png")

        line1 = plt.plot(
            analytical_sln[10], z, "-", color="red", label="Analytical"
        )
        line2 = plt.plot(
            simulated_sln[10], z, "-.", color="blue", label="MODFLOW 6"
        )
        # 50th transient stress period
        plt.plot(analytical_sln[50], z, "-", color="red")
        plt.plot(simulated_sln[50], z, "-.", color="blue")
        # last stress period
        plt.plot(analytical_sln[100], z, "-", color="red")
        plt.plot(simulated_sln[100], z, "-.", color="blue")
        # add labels
        plt.text(11.0, 0.85, "1 day", fontsize=10)
        plt.text(12.0, 1.65, "10 days", fontsize=10)
        plt.text(14.0, 2.90, "50 days", fontsize=10)
        plt.text(16.0, 4.00, "100 days", fontsize=10)

        plt.gca().invert_yaxis()
        plt.xlabel("$Energy Flux, -$")
        plt.ylabel("$Depth, m$")
        plt.minorticks_on()
        plt.axhline(y=0.0)
        plt.legend(loc="lower right", frameon=False)
        plt.savefig(os.path.join(ws, "fit_view.png"), format="png")


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(cases)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
