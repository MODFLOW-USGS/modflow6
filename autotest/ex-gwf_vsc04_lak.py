# Simple single lake model.  Lake cut into top two layers.  Model
# is loosely based on one of the MT3D-USGS test problems.  This test
# developed to isolate lake-aquifer interaction; no SFR or other advanced
# packages.  Problem set up to have groundwater pass through the lake:
# gw inflow on the left side, gw outflow on the right side of the lake.


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
import config

ex = ["vsc-lak01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", "examples", s))

# Model units
length_units = "m"
time_units = "days"

# model domain and grid definition
delr = [
    76.2,
    304.8,
    304.8,
    304.8,
    304.8,
    304.8,
    152.4,
    152.4,
    152.4,
    152.4,
    152.4,
    304.8,
    304.8,
    304.8,
    304.8,
    304.8,
    76.2
]

delc = [
    76.2,
    304.8,
    304.8,
    304.8,
    304.8,
    304.8,
    152.4,
    152.4,
    152.4,
    152.4,
    152.4,
    304.8,
    304.8,
    304.8,
    304.8,
    304.8,
    76.2
]

fixedstrthds = [
    35.052,
    34.9267,
    34.7216,
    34.5062,
    34.2755,
    34.0237,
    33.8143,
    33.6657,
    33.5077,
    33.3394,
    33.1599,
    32.8728,
    32.4431,
    31.9632,
    31.4353,
    30.8627,
    30.48
]

nrow = len(delc)
ncol = len(delr)
top = np.ones((nrow, ncol)) * 35.6616
bot1 = np.ones_like(top) * 32.6136
bot2 = np.ones_like(top) * 29.5656
bot3 = np.ones_like(top) * 26.5176
bot4 = np.ones_like(top) * 23.4696
bot5 = np.ones_like(top) * 20.4216
botm = np.array([bot1, bot2, bot3, bot4, bot5])
nlay = botm.shape[0]
ibound = np.ones_like(botm)

# deactive gw cells where lake cells are active
ibound[0, 6:11, 6:11] = 0  # layer 1
ibound[1, 7:10, 7:10] = 0  # layer 2

strthd = np.zeros_like(ibound)
for j in np.arange(ncol):
    strthd[:, :, j] = fixedstrthds[j]

# setup lake array
lakibnd = np.zeros_like(ibound)
lakibnd[0] = 1 - ibound[0]  # layer 1
lakibnd[1] = 1 - ibound[1]  # layer 2

# NPF parameters
k11 = 9.144  # = 30 ft/day
k33 = 0.9144  # = 30 ft/day
ss = 3e-4
sy = 0.20
hani = 1
laytyp = 1

# Package boundary conditions
chdl = 35.052
chdr = 30.48
viscref = 8.904e-4

# time params
transient = {0: True}
nstp = [100]
tsmult = [1.02]
perlen = [5000]

# solver params
nouter, ninner = 1000, 300
hclose, rclose, relax = 1e-3, 1e-4, 0.97

# Transport related parameters
al = 1  # longitudinal dispersivity ($m$)
ath1 = al  # horizontal transverse dispersivity
atv = al  # vertical transverse dispersivity
mixelm = 0  # Upstream vs TVD (Upstream selected)
initial_temperature = 35.0  # Initial temperature (unitless)
porosity = 0.20  # porosity (unitless)
K_therm = 2.0  # Thermal conductivity  # ($W/m/C$)
rho_water = 1000  # Density of water ($kg/m^3$)
rho_solids = 2650  # Density of the aquifer material ($kg/m^3$)
C_p_w = 4180  # Heat Capacity of water ($J/kg/C$)
C_s = 880  # Heat capacity of the solids ($J/kg/C$)
D_m = K_therm / (porosity * rho_water * C_p_w)
rhob = (1 - porosity) * rho_solids  # Bulk density ($kg/m^3$)
K_d = C_s / (rho_water * C_p_w)  # Partitioning coefficient ($m^3/kg$)
leftTemp = 30.0  # Temperature of inflow from left constant head ($C$)

# Viscosity related parameters
tviscref = 20.0

# MODFLOW 6 flopy GWF & GWT simulation object (sim) is returned
#
def build_model(idx, sim_folder='vsc_wLAK'):
    print("Building model...{}".format(sim_folder))

    # generate names for each model
    name = "vsc"
    gwfname = "gwf-" + name + str(idx) + "-lak"
    gwtname = "gwt-" + name + str(idx) + "-lak"

    sim_ws = os.path.join(exdirs[0], sim_folder)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=sim_ws, exe_name=config.mf6_exe
    )

    tdis_rc = []
    for i in range(len(nstp)):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    flopy.mf6.ModflowTdis(
        sim, nper=len(nstp), perioddata=tdis_rc, time_units=time_units
    )

    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        save_flows=True,
        newtonoptions="newton"
    )

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="cooley",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwfname),
    )
    sim.register_ims_package(ims, [gwfname])

    # Instantiate discretization package
    flopy.mf6.ModflowGwfdis(
        gwf,
        length_units=length_units,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=ibound,
        filename='{}.dis'.format(gwfname)
    )

    # Instantiate node property flow package
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        icelltype=1,  # >0 means saturated thickness varies with computed head
        k=k11,
        k33=k33
    )

    # Instantiate storage package
    flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=ss,
        sy=sy,
        transient=transient
    )

    # Instantiate initial conditions package
    flopy.mf6.ModflowGwfic(
        gwf,
        strt=strthd
    )

    # Instantiate viscosity package
    vsc_filerecord = "{}.vsc.bin".format(gwfname)
    vsc_pd = [(0, 0.0, tviscref, gwtname, "TEMPERATURE")]
    flopy.mf6.ModflowGwfvsc(
        gwf,
        viscref=viscref,
        viscosity_filerecord=vsc_filerecord,
        viscosityfuncrecord=[('nonlinear', 10.0, 248.37, 133.15)],
        nviscspecies=len(vsc_pd),
        packagedata=vsc_pd,
        pname='vsc',
        filename="{}.vsc".format(gwfname)
    )

    # Instantiate output control package
    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 17, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # Instantiate constant head package
    # (for driving gw flow from left to right)
    chdlistl = []
    chdlistr = []
    for k in np.arange(nlay):
        for i in np.arange(nrow):
            # left side
            if botm[k, i, 0] <= chdl:
                chdlistl.append([(k, i, 0), chdl, leftTemp])
            # right side
            if botm[k, i, -1] <= chdr:
                chdlistr.append([(k, i, ncol - 1), chdr, 10.0])

    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=chdlistl,
        print_input=True,
        print_flows=True,
        save_flows=False,
        pname="CHD-L",
        auxiliary="TEMPERATURE",
        filename=f"{gwfname}.left.chd",
    )

    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=chdlistr,
        print_input=True,
        print_flows=True,
        save_flows=False,
        pname="CHD-R",
        auxiliary="TEMPERATURE",
        filename=f"{gwfname}.right.chd",
    )

    # Instantiate lake package

    lakeconnectiondata = []
    nlakecon = [0]  # Expand this to [0, 0, ...] for each additional lake
    ilakconn = -1
    lak_leakance = 0.1
    for k in [0, 1]:
        for i in range(nrow):
            for j in range(ncol):
                if lakibnd[k, i, j] == 0:
                    continue
                else:
                    ilak = int(lakibnd[k, i, j] - 1)
                    # back
                    if i > 0:
                        if lakibnd[k, i - 1, j] == 0 and ibound[k, i - 1, j] == 1:
                            ilakconn += 1
                            # by setting belev==telev, MF6 will automatically re-assign elevations based on cell dimensions
                            # <lakeno>     <iconn> <cellid(ncelldim)>   <claktype>      <bedleak> <belev> <telev> <connlen> <connwidth>
                            h = [ilak, ilakconn, (k, i - 1, j), 'horizontal', lak_leakance, 0.0, 0.0, delc[i] / 2., delr[j]]
                            lakeconnectiondata.append(h)

                    # left
                    if j > 0:
                        if lakibnd[k, i, j - 1] == 0 and ibound[k, i, j - 1] == 1:
                            ilakconn += 1
                            h = [ilak, ilakconn, (k, i, j - 1), 'horizontal', lak_leakance, 0.0, 0.0, delr[j] / 2., delc[i]]
                            lakeconnectiondata.append(h)

                    # right
                    if j < ncol - 1:
                        if lakibnd[k, i, j + 1] == 0 and ibound[k, i, j + 1] == 1:
                            ilakconn += 1
                            h = [ilak, ilakconn, (k, i, j + 1), 'horizontal', lak_leakance, 0.0, 0.0, delr[j] / 2., delc[i]]
                            lakeconnectiondata.append(h)

                    # front
                    if i < nrow - 1:
                        if lakibnd[k, i + 1, j] == 0 and ibound[k, i + 1, j] == 1:
                            ilakconn += 1
                            h = [ilak, ilakconn, (k, i + 1, j), 'horizontal', lak_leakance, 0.0, 0.0, delc[i] / 2., delr[j]]
                            lakeconnectiondata.append(h)

                # vertical
                if lakibnd[k, i, j] == 1 and ibound[k + 1, i, j] == 1:
                    ilakconn += 1
                    v = [ilak, ilakconn, (k + 1, i, j), 'vertical', lak_leakance, 0.0, 0.0, 0.0, 0.0]
                    lakeconnectiondata.append(v)

    strtStg = 33.75
    lakpackagedata = [[0, strtStg, len(lakeconnectiondata), 4.0, 'lake1']]
    lak_pkdat_dict = {'filename': "lak_pakdata.in", 'data': lakpackagedata}

    lakeperioddata = {0: [(0, 'STATUS', 'CONSTANT'),  #RAINFALL 0.005 & 0.00504739035
                          (0, 'STAGE', 33.5)]}

    lak_obs = {'{}.lakeobs'.format(gwfname): [('lakestage', 'stage', 'lake1'),
                                              ('gwexchng', 'lak', 'lake1')]}
    lak = flopy.mf6.ModflowGwflak(
        gwf,
        auxiliary="TEMPERATURE",
        time_conversion=86400.0,
        print_stage=True,
        print_flows=True,
        budget_filerecord=gwfname + '.lak.bud',
        length_conversion=1.0,
        mover=False,
        pname='LAK-1',
        boundnames=True,
        nlakes=len(lakpackagedata),
        noutlets=0,
        packagedata=lak_pkdat_dict,
        connectiondata=lakeconnectiondata,
        perioddata=lakeperioddata,
        observations=lak_obs,
        filename='{}.lak'.format(gwfname)
    )

    # pull in th etabfile defining the lake stage, vol, & surface area
    fname = os.path.join('data', 'vsc04-laktab', 'stg-vol-surfarea.csv')
    tabinput = []
    with open(fname, 'r') as f:
        # peel off the hdr line
        hdr = next(f)
        for line in f:
            m_arr = line.strip().split(',')
            #                        <stage>, <volume>,  <sarea>,
            tabinput.append([float(m_arr[0]), m_arr[1], m_arr[2]])

    tab6_filename = '{}.laktab'.format(gwfname)
    flopy.mf6.ModflowUtllaktab(
        gwf,
        nrow=len(tabinput),
        ncol=3,
        table=tabinput,
        filename=tab6_filename,
        pname='LAK_tab',
        parent_file=lak
    )

    # create gwt model
    # ----------------
    gwt = flopy.mf6.ModflowGwt(
        sim,
        modelname=gwtname,
        model_nam_file='{}.nam'.format(gwtname)
    )
    gwt.name_file.save_flows = True

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
        filename=f"{gwtname}.ims",
    )
    sim.register_ims_package(imsgwt, [gwt.name])

    # Instantiating MODFLOW 6 transport discretization package
    flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=ibound,
        filename='{}.dis'.format(gwtname)
    )

    # Instantiating MODFLOW 6 transport initial concentrations
    strtconc = leftTemp
    flopy.mf6.ModflowGwtic(
        gwt,
        strt=strtconc,
        filename='{}.ic'.format(gwtname)
    )

    # Instantiate mobile storage and transfer package
    sto = flopy.mf6.ModflowGwtmst(
        gwt, porosity=porosity, filename=f"{gwtname}.sto"
    )

    # Instantiating MODFLOW 6 transport advection package
    if mixelm == 0:
        scheme = 'UPSTREAM'
    elif mixelm == -1:
        scheme = 'TVD'
    else:
        raise Exception()
    flopy.mf6.ModflowGwtadv(gwt,
                            scheme=scheme,
                            filename='{}.adv'.format(gwtname)
    )

    # Instantiate dispersion package
    flopy.mf6.ModflowGwtdsp(
        gwt,
        alh=al,
        ath1=ath1,
        atv=atv,
        filename='{}.dsp'.format(gwtname)
    )

    # Instantiate source/sink mixing package
    sourcerecarray = [
        ("CHD-L", "AUX", "TEMPERATURE"),
        ("CHD-R", "AUX", "TEMPERATURE"),
    ]
    flopy.mf6.ModflowGwtssm(
        gwt,
        sources=sourcerecarray,
        filename=f"{gwtname}.ssm"
    )

    # Instantiating MODFLOW 6 transport output control package
    flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord='{}.cbc'.format(gwtname),
        concentration_filerecord='{}.ucn'.format(
            gwtname),
        concentrationprintrecord=[
            ('COLUMNS', 17, 'WIDTH', 15, 'DIGITS', 6, 'GENERAL')],
        saverecord=[('CONCENTRATION', 'ALL'), ('BUDGET', 'ALL')],
        printrecord=[('CONCENTRATION', 'ALL'), ('BUDGET', 'ALL')],
        filename='{}.oc'.format(gwtname)
    )

    # Instantiating MODFLOW 6 lake transport (lkt) package
    lktpackagedata = [(0, 4., 'lake1')]

    lktperioddata = {0: [(0, 'STATUS', 'CONSTANT'),
                         (0, 'CONCENTRATION', 4.0)]}

    # note: for specifying lake number, use fortran indexing!
    lkt_obs = {'{}.lakobs'.format(gwtname): [('resTemp', 'concentration', 1),
                                             ('resGwMassExchng', 'lkt', 'lake1')]}

    flopy.mf6.ModflowGwtlkt(
        gwt,  # Set time_conversion for use with Manning's eqn.
        flow_package_name='LAK-1',
        flow_package_auxiliary_name='TEMPERATURE',
        budget_filerecord=gwtname + '.lkt.bud',
        boundnames=True,
        save_flows=True,
        print_input=True,
        print_flows=False,
        print_concentration=True,
        packagedata=lktpackagedata,
        lakeperioddata=lktperioddata,
        observations=lkt_obs,
        pname='LKT-1',
        filename='{}.lkt'.format(gwtname)
    )

    # GWF GWT exchange
    flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename=f"{name}.gwfgwt",
    )

    return sim


def write_model(sim, silent=True):
    if config.writeModel:
        sim.write_simulation(silent=silent)
    return


def scenario(idx, silent=True):
    sim = build_model(idx)
    write_model(sim, silent=silent)


# nosetest - exclude block from this nosetest to the next nosetest
def test_01():
    scenario(0, silent=False)

# nosetest end

if __name__ == "__main__":

    # ### MT3D-USGS LKT test problem adapted for testing vsc in/out of lake
    scenario(0)

