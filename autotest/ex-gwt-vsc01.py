# ## Test problem for VSC 
#
# Model domain is lifted from the Henry Problem
#

# ### VSC Problem Setup

# Imports

import os
import sys
import matplotlib.pyplot as plt
import flopy
import numpy as np

# Append to system path to include the common subdirectory

sys.path.append(os.path.join("..", "common"))

# Import common functionality

import config
from figspecs import USGSFigure

mf6exe = os.path.abspath(config.mf6_exe)


# Set figure properties specific to this problem

figure_size = (6, 4)

# Base simulation and model name and workspace

ws = os.path.join('temp', 'examples', 'vsc-henry')

# Scenario parameters - make sure there is at least one blank line before next item

parameters = {
    "ex-gwt-vsc-a": {"vsc_on": False,},
    "ex-gwt-vsc-b": {"vsc_on": True,},
}

# Model units

length_units = "cm"
time_units = "seconds"

# Table of model parameters

nper = 1  # Number of periods
nstp = 500  # Number of time steps
perlen = 0.5  # Simulation time length ($d$)
nlay = 1  # Number of layers
nrow = 40  # Number of rows
ncol = 80  # Number of columns
system_length = 2.0  # Length of system ($m$)
delr = 0.025  # Column width ($m$)
delc = 1.0  # Row width ($m$)
delv = 0.025  # Layer thickness
top = 1.0  # Top of the model ($m$)
hydraulic_conductivity = 864.0  # Hydraulic conductivity ($m d^{-1}$)
initial_temperature = 35.0  # Initial temperature (unitless)
porosity = 0.26  # porosity (unitless)
K_therm = 2.0  # Thermal conductivity  # ($W/m/C$)
rho_water = 1000  # Density of water ($kg/m^3$)
rho_solids = 2650  # Density of the aquifer material ($kg/m^3$)
C_p_w = 4180  # Heat Capacity of water ($J/kg/C$)
C_s = 880 # Heat capacity of the solids ($J/kg/C$)
D_m = K_therm / (porosity * rho_water * C_p_w)
rhob = (1 - porosity) * rho_solids  # Bulk density ($kg/m^3$)
K_d = C_s / (rho_water * C_p_w)  # Partitioning coefficient ($m^3/kg$)
inflow = 5.7024  # ($m^3/d$)

botm = [top - k * delv for k in range(1, nlay + 1)]

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-10, 1e-6, 0.97


# ### Functions to build, write, run, and plot models
#
# MODFLOW 6 flopy GWF simulation object (sim) is returned
#


def build_model(sim_folder, vsc_on):
    print("Building model...{}".format(sim_folder))

    # generate names for each model
    name = "vsc01"
    gwfname = "gwf_" + name
    gwtname = "gwt_" + name

    sim_ws = os.path.join(ws, sim_folder)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=sim_ws, exe_name=config.mf6_exe
    )
    tdis_ds = ((perlen, nstp, 1.0),)
    flopy.mf6.ModflowTdis(
        sim, nper=nper, perioddata=tdis_ds, time_units=time_units
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)
    ims = flopy.mf6.ModflowIms(
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
        filename="{}.ims".format(gwfname),
    )
    sim.register_ims_package(ims, [gwfname])
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
    )
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        icelltype=0,
        k=hydraulic_conductivity,
    )
    flopy.mf6.ModflowGwfic(gwf, strt=0.0)

    # Attempting to instantiate VSC after creating a definition file
    vsc_filerecord = "{}.vsc.bin".format(gwfname)
    vsc_pd = [(0, 0.0, 20.0, gwtname, "temperature")]
    flopy.mf6.ModflowGwfvsc(
        gwf,
        viscref=8.904e-4,
        viscosity_filerecord=vsc_filerecord,
        viscosityfuncrecord=[('nonlinear', 10.0, 248.37, 133.16)],
        nviscspecies=len(vsc_pd),
        packagedata=vsc_pd,
        pname='vsc',
        filename="{}.vsc".format(gwfname)
    )

    # Instantiating GHB
    ghbcond = hydraulic_conductivity * delv * delc / (0.5 * delr)
    ghbspd = [[(k, 0, ncol - 1), top, ghbcond, 35.0] for k in range(nlay)]
    flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=ghbspd,
        pname="GHB-1",
        auxiliary="temperature",
    )

    # Instantiating WEL
    welspd = [[(k, 0, 0), inflow / nlay, 0.0] for k in range(nlay)]
    flopy.mf6.ModflowGwfwel(
        gwf,
        stress_period_data=welspd,
        pname="WEL-1",
        auxiliary="temperature",
    )
    head_filerecord = "{}.hds".format(name)
    budget_filerecord = "{}.bud".format(name)

    # Instatiatingi OC
    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=head_filerecord,
        budget_filerecord=budget_filerecord,
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # Setup the GWT model for simulating heat transport
    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname)
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
        filename="{}.ims".format(gwtname),
    )
    sim.register_ims_package(imsgwt, [gwtname])
    flopy.mf6.ModflowGwtdis(
        gwt,
        length_units=length_units,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    flopy.mf6.ModflowGwtmst(
        gwt,
        porosity=porosity,
        sorption='linear',
        bulk_density=rhob,
        distcoef=K_d,
        pname="MST-1",
        filename="{}.mst".format(gwtname),
    )

    flopy.mf6.ModflowGwtic(gwt, strt=initial_temperature)
    flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")
    flopy.mf6.ModflowGwtdsp(gwt, xt3d_off=True, diffc=D_m)
    sourcerecarray = [
        ("GHB-1", "AUX", "TEMPERATURE"),
        ("WEL-1", "AUX", "TEMPERATURE"),
    ]
    flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)
    flopy.mf6.ModflowGwtoc(
        gwt,
        concentration_filerecord="{}.ucn".format(gwtname),
        saverecord=[("CONCENTRATION", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )
    flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname
    )
    return sim


# Function to write model files


def write_model(sim, silent=True):
    if config.writeModel:
        sim.write_simulation(silent=silent)
    return


# Function to run the model
# True is returned if the model runs successfully


@config.timeit
def run_model(sim, silent=True):
    success = True
    if config.runModel:
        success = False
        success, buff = sim.run_simulation(silent=silent)
        if not success:
            print(buff)
    return success


# Function to plot the model results


def plot_conc(sim, idx):
    fs = USGSFigure(figure_type="map", verbose=False)
    sim_name = list(parameters.keys())[idx]
    sim_ws = os.path.join(ws, sim_name)
    gwf = sim.get_model("flow")
    gwt = sim.get_model("trans")

    fig = plt.figure(figsize=figure_size)
    fig.tight_layout()

    # get MODFLOW 6 temperature
    conc = gwt.output.temperature().get_data()

    ax = fig.add_subplot(1, 1, 1, aspect="equal")
    pxs = flopy.plot.PlotCrossSection(model=gwf, ax=ax, line={"row": 0})
    pxs.plot_array(conc, cmap="jet")
    levels = [35 * f for f in [0.01, 0.1, 0.5, 0.9, 0.99]]
    cs = pxs.contour_array(
        conc, levels=levels, colors="w", linewidths=1.0, linestyles="-"
    )
    ax.set_xlabel("x position (m)")
    ax.set_ylabel("z position (m)")
    plt.clabel(cs, fmt="%4.2f", fontsize=5)

    # save figure
    if config.plotSave:
        fpth = os.path.join(
            "..", "figures", "{}-conc{}".format(sim_name, config.figure_ext)
        )
        fig.savefig(fpth)
    return


def plot_results(sim, idx):
    if config.plotModel:
        plot_conc(sim, idx)
    return


# Function that wraps all of the steps for each scenario
#
# 1. build_model,
# 2. write_model,
# 3. run_model, and
# 4. plot_results.
#


def scenario(idx, silent=True):
    key = list(parameters.keys())[idx]
    parameter_dict = parameters[key]
    sim = build_model(key, **parameter_dict)
    write_model(sim, silent=silent)
    success = run_model(sim, silent=silent)
    if success:
        plot_results(sim, idx)


# nosetest - exclude block from this nosetest to the next nosetest
def test_01():
    scenario(0, silent=False)


def test_02():
    scenario(1, silent=False)


# nosetest end

if __name__ == "__main__":
    # ### Henry Problem

    # Scenario 1 - Classic henry problem

    scenario(0)

    # Scenario 2 - Modified Henry problem with half the inflow rate

    scenario(1)
