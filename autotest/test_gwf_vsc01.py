# ## Test problem for VSC
#
# Uses constant head and general-head boundaries on the left and right
# sides of the model domain, respectively, to drive flow from left to
# right.  Tests that head-dependent boundary conditions are properly
# # accounting for viscosity when VSC is active.
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

# Scenario parameters - make sure there is at least one blank line before next item

hyd_cond = [1205.49396942506, 864.0]  # Hydraulic conductivity ($m d^{-1}$)
parameters = {
    "no-vsc01-bnd": {"vsc_on": False, "hydraulic_conductivity": hyd_cond[0]},
    "vsc01-bnd": {"vsc_on": True, "hydraulic_conductivity": hyd_cond[1]},
    "no-vsc01-k": {"vsc_on": False, "hydraulic_conductivity": hyd_cond[1]},
}

# Model units

length_units = "cm"
time_units = "seconds"

# Table of model parameters

nper = 1  # Number of periods
nstp = 500  # Number of time steps
perlen = 0.5  # Simulation time length ($d$)
nlay = 1  # Number of layers
nrow = 10  # Number of rows
ncol = 80  # Number of columns
system_length = 2.0  # Length of system ($m$)
delr = 1.0  # Column width ($m$)
delc = 1.0  # Row width ($m$)
delv = 1.0  # Layer thickness
top = 1.0  # Top of the model ($m$)
initial_temperature = 35.0  # Initial temperature (unitless)
porosity = 0.26  # porosity (unitless)
K_therm = 2.0  # Thermal conductivity  # ($W/m/C$)
rho_water = 1000  # Density of water ($kg/m^3$)
rho_solids = 2650  # Density of the aquifer material ($kg/m^3$)
C_p_w = 4180  # Heat Capacity of water ($J/kg/C$)
C_s = 880  # Heat capacity of the solids ($J/kg/C$)
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


def build_model(key, vsc_on, hydraulic_conductivity):
    print("Building model...{}".format(key))

    # Base simulation and model name and workspace
    ws = os.path.join("temp", "examples", key)

    # generate names for each model
    name = "vsc01"
    gwfname = "gwf-" + key
    gwtname = "gwt-" + key

    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws, exe_name=mf6exe)
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

    if vsc_on:
        # Instantiate viscosity (VSC) package
        vsc_filerecord = "{}.vsc.bin".format(gwfname)
        vsc_pd = [(0, 0.0, 20.0, gwtname, "temperature")]
        flopy.mf6.ModflowGwfvsc(
            gwf,
            viscref=8.904e-4,
            viscosity_filerecord=vsc_filerecord,
            viscosityfuncrecord=[("nonlinear", 10.0, 248.37, 133.16)],
            nviscspecies=len(vsc_pd),
            packagedata=vsc_pd,
            pname="vsc",
            filename="{}.vsc".format(gwfname),
        )

    # Instantiating GHB
    ghbcond = hydraulic_conductivity * delv * delc / (0.5 * delr)
    ghbspd = [
        [(0, i, ncol - 1), top, ghbcond, initial_temperature]
        for i in range(nrow)
    ]
    flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=ghbspd,
        pname="GHB-1",
        auxiliary="temperature",
    )

    # Instantiating CHD
    chdspd = [[(0, i, 0), 2.0, initial_temperature] for i in range(nrow)]
    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=chdspd,
        pname="CHD-1",
        auxiliary="temperature",
    )

    # Instatiating OC
    head_filerecord = "{}.hds".format(key)
    budget_filerecord = "{}.bud".format(key)
    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=head_filerecord,
        budget_filerecord=budget_filerecord,
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # Setup the GWT model for simulating heat transport
    # -------------------------------------------------
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
        sorption="linear",
        bulk_density=rhob,
        distcoef=K_d,
        pname="MST-1",
        filename="{}.mst".format(gwtname),
    )

    flopy.mf6.ModflowGwtic(gwt, strt=initial_temperature)

    flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")

    flopy.mf6.ModflowGwtdsp(gwt, xt3d_off=True, diffc=D_m)

    sourcerecarray = [
        ("CHD-1", "AUX", "TEMPERATURE"),
        ("GHB-1", "AUX", "TEMPERATURE"),
    ]
    flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)

    flopy.mf6.ModflowGwtoc(
        gwt,
        concentration_filerecord="{}.ucn".format(gwtname),
        saverecord=[("CONCENTRATION", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    flopy.mf6.ModflowGwfgwt(
        sim, exgtype="GWF6-GWT6", exgmnamea=gwfname, exgmnameb=gwtname
    )

    return sim


# Function to write and run model files
def write_and_run_model(sim, key, silent=True):
    sim.write_simulation(silent=silent)

    success, buff = sim.run_simulation(silent=False)
    errmsg = f"simulation did not terminate successfully\n{buff}"
    assert success, errmsg

    # digest model budgets
    simpath = sim.simulation_data.mfpath.get_sim_path()

    modbud = key + ".bud"
    fpth = os.path.join(simpath, modbud)
    budobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    outbud = budobj.get_data(text="             GHB")

    return outbud[-1]


def confirm_run_results(
    modname1, modname2, modname3, no_vsc_bud, with_vsc_bud, low_k_bud
):

    # Sum up total flow leaving the model through the GHB boundary
    no_vsc_bud_last = np.array(no_vsc_bud.tolist())
    no_with_vsc_bud_last = np.array(with_vsc_bud.tolist())
    no_low_k_bud_last = np.array(low_k_bud.tolist())

    model1_exit = no_vsc_bud_last[:, 2].sum()
    model2_exit = no_with_vsc_bud_last[:, 2].sum()
    model3_exit = no_low_k_bud_last[:, 2].sum()

    # Ensure models 1 & 2 give nearly identical results
    assert np.allclose(model1_exit, model2_exit, atol=1e-3), (
        "VSC results not right between models: "
        + modname1
        + " and "
        + modname2
    )

    # Ensure the flow leaving model 3 is less than that which leaves model 2
    assert abs(model2_exit) > abs(model3_exit), (
        "VSC results not right between models: "
        + modname2
        + " and "
        + modname3
    )


def scenario(idx, silent=True):
    # Three model runs that test model flows with and without
    # viscosity active

    # Model Run 1 (Fake the effects of viscosity with pre-specified K)
    # Model Run 2 (Account for the effects of viscosity)
    # Model Run 3 (Ensure that base K scenario results in less flow)
    # ---------------------------------------------------------
    key = list(parameters.keys())[idx]
    parameter_dict = parameters[key]
    sim = build_model(key, **parameter_dict)
    cbc_bud = write_and_run_model(sim, key, silent=silent)

    return key, cbc_bud


# nosetest - exclude block from this nosetest to the next nosetest
def test_01():
    # Compare model runs with and without viscosity package active
    # Model 1 - no viscosity, but with elevated K's that mimic viscosity
    #           adjusted K values.
    modname1, bnd_scen1_bud = scenario(0)

    # Model 2 - include viscosity
    modname2, bnd_scen2_bud = scenario(1)

    # Model 3 - no viscosity with basecase K, flows should be reduced
    modname3, bnd_scen3_bud = scenario(2)

    # Flow through model should be the same in models 1 & 2 based on
    # specified K's.  That is, the second model's adjusted K should
    # be equal to the pre-specified K in model 1.  The third model uses
    # the base K of 864 m/d without simulating viscosity effects and
    # should result in less flow through the model.
    confirm_run_results(
        modname1,
        modname2,
        modname3,
        bnd_scen1_bud,
        bnd_scen2_bud,
        bnd_scen3_bud,
    )


# nosetest end

if __name__ == "__main__":
    # Compare model runs with and without viscosity package active
    # Model 1 - no viscosity, but with elevated K's that mimic viscosity
    #           adjusted K values.
    modname1, bnd_scen1_bud = scenario(0)

    # Model 2 - include viscosity
    modname2, bnd_scen2_bud = scenario(1)

    # Model 3 - no viscosity with basecase K, flows should be reduced
    modname3, bnd_scen3_bud = scenario(2)

    # Flow through model should be the same in models 1 & 2 based on
    # specified K's.  That is, the second model's adjusted K should
    # be equal to the pre-specified K in model 1.  The third model uses
    # the base K of 864 m/d without simulating viscosity effects and
    # should result in less flow through the model.
    confirm_run_results(
        modname1,
        modname2,
        modname3,
        bnd_scen1_bud,
        bnd_scen2_bud,
        bnd_scen3_bud,
    )
