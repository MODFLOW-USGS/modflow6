# ## Test problem for VSC
#
# Uses general-head and drain boundaries on the left and right
# sides of the model domain, respectively, to drive flow from left to
# right.  Tests that head-dependent boundary conditions are properly
# accounting for viscosity when VSC is active.  Similar to gwf-vsc01-bnd
# but employs head-dependent boundary on the left and right side of the
# model

# Imports

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

# Import common functionality
from framework import testing_framework
from simulation import Simulation

# Setup scenario input
hyd_cond = [1205.49396942506, 864.0]  # Hydraulic conductivity (m/d)
ex = ["no-vsc02-bnd", "vsc02-bnd", "no-vsc02-k"]
viscosity_on = [False, True, False]
hydraulic_conductivity = [hyd_cond[0], hyd_cond[1], hyd_cond[1]]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

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

#
# MODFLOW 6 flopy GWF simulation object (sim) is returned
#


def build_model(idx, dir):
    # Base simulation and model name and workspace
    ws = dir
    name = ex[idx]

    print("Building model...{}".format(name))

    # generate names for each model
    gwfname = "gwf-" + name
    gwtname = "gwt-" + name

    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=ws, exe_name="mf6", version="mf6"
    )

    # Instantiating time discretization
    tdis_ds = ((perlen, nstp, 1.0),)
    flopy.mf6.ModflowTdis(
        sim, nper=nper, perioddata=tdis_ds, time_units=time_units
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)

    # Instantiating solver
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

    # Instantiating DIS
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

    # Instantiating NPF
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        icelltype=0,
        k=hydraulic_conductivity[idx],
    )
    flopy.mf6.ModflowGwfic(gwf, strt=0.0)

    # Instantiating VSC
    if viscosity_on[idx]:
        # Instantiate viscosity (VSC) package
        vsc_filerecord = "{}.vsc.bin".format(gwfname)
        vsc_pd = [(0, 0.0, 20.0, gwtname, "temperature")]
        flopy.mf6.ModflowGwfvsc(
            gwf,
            viscref=8.904e-4,
            viscosity_filerecord=vsc_filerecord,
            thermal_formulation="nonlinear",
            thermal_a2=10.0,
            thermal_a3=248.37,
            thermal_a4=133.16,
            nviscspecies=len(vsc_pd),
            packagedata=vsc_pd,
            pname="vsc",
            filename="{}.vsc".format(gwfname),
        )

    # Instantiating GHB
    ghbcond = hydraulic_conductivity[idx] * delv * delc / (0.5 * delr)
    ghbspd = [
        [(0, i, 0), top + 3, ghbcond, initial_temperature] for i in range(nrow)
    ]
    flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=ghbspd,
        pname="GHB-1",
        auxiliary="temperature",
    )

    # Instantiating DRN
    drnspd = [
        [(0, i, ncol - 1), top, 1.2 * ghbcond, initial_temperature]
        for i in range(nrow)
    ]
    flopy.mf6.ModflowGwfdrn(
        gwf,
        stress_period_data=drnspd,
        pname="DRN-1",
        auxiliary="temperature",
    )

    # Instatiatingi OC
    head_filerecord = "{}.hds".format(gwfname)
    budget_filerecord = "{}.bud".format(gwfname)
    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=head_filerecord,
        budget_filerecord=budget_filerecord,
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # Setup the GWT model for simulating heat transport
    # -------------------------------------------------
    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname)

    # Instantiating solver for GWT
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

    # Instantiating DIS for GWT
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

    # Instantiating MST for GWT
    flopy.mf6.ModflowGwtmst(
        gwt,
        porosity=porosity,
        sorption="linear",
        bulk_density=rhob,
        distcoef=K_d,
        pname="MST-1",
        filename="{}.mst".format(gwtname),
    )

    # Instantiating IC for GWT
    flopy.mf6.ModflowGwtic(gwt, strt=initial_temperature)

    # Instantiating ADV for GWT
    flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")

    # Instantiating DSP for GWT
    flopy.mf6.ModflowGwtdsp(gwt, xt3d_off=True, diffc=D_m)

    # Instantiating SSM for GWT
    sourcerecarray = [
        ("GHB-1", "AUX", "TEMPERATURE"),
        ("DRN-1", "AUX", "TEMPERATURE"),
    ]
    flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)

    # Instantiating OC for GWT
    flopy.mf6.ModflowGwtoc(
        gwt,
        concentration_filerecord="{}.ucn".format(gwtname),
        saverecord=[("CONCENTRATION", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    # Instantiating GWF/GWT Exchange
    flopy.mf6.ModflowGwfgwt(
        sim, exgtype="GWF6-GWT6", exgmnamea=gwfname, exgmnameb=gwtname
    )
    return sim, None


def eval_results(sim):
    print("evaluating results...")

    # read flow results from model
    name = ex[sim.idxsim]
    gwfname = "gwf-" + name

    fname = gwfname + ".bud"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    budobj = flopy.utils.CellBudgetFile(fname, precision="double")
    outbud = budobj.get_data(text="             GHB")

    if sim.idxsim == 0:
        no_vsc_bud_last = np.array(outbud[-1].tolist())
        np.savetxt(
            os.path.join(os.path.dirname(exdirs[sim.idxsim]), "mod1reslt.txt"),
            no_vsc_bud_last,
        )

    elif sim.idxsim == 1:
        with_vsc_bud_last = np.array(outbud[-1].tolist())
        np.savetxt(
            os.path.join(os.path.dirname(exdirs[sim.idxsim]), "mod2reslt.txt"),
            with_vsc_bud_last,
        )

    elif sim.idxsim == 2:
        no_vsc_low_k_bud_last = np.array(outbud[-1].tolist())
        np.savetxt(
            os.path.join(os.path.dirname(exdirs[sim.idxsim]), "mod3reslt.txt"),
            no_vsc_low_k_bud_last,
        )

    # if all 3 models have run, check relative results
    if sim.idxsim == 2:
        f1 = os.path.join(os.path.dirname(exdirs[sim.idxsim]), "mod1reslt.txt")
        if os.path.isfile(f1):
            no_vsc_bud_last = np.loadtxt(f1)
            os.remove(f1)

        f2 = os.path.join(os.path.dirname(exdirs[sim.idxsim]), "mod2reslt.txt")
        if os.path.isfile(f2):
            with_vsc_bud_last = np.loadtxt(f2)
            os.remove(f2)

        f3 = os.path.join(os.path.dirname(exdirs[sim.idxsim]), "mod3reslt.txt")
        if os.path.isfile(f3):
            no_vsc_low_k_bud_last = np.loadtxt(f3)
            os.remove(f3)

        model1_exit = no_vsc_bud_last[:, 2].sum()
        model2_exit = with_vsc_bud_last[:, 2].sum()
        model3_exit = no_vsc_low_k_bud_last[:, 2].sum()

        # Ensure models 1 & 2 give nearly identical results
        assert np.allclose(model1_exit, model2_exit, atol=1e-3), (
            "Flow in models "
            + exdirs[0]
            + " and "
            + exdirs[1]
            + " should be equal, but are not."
        )

        # Ensure the flow leaving model 3 is less than what leaves model 2
        assert abs(model2_exit) > abs(model3_exit), (
            "Exit flow from model "
            + exdirs[1]
            + " should be greater than flow existing "
            + exdirs[2]
            + ", but it is not."
        )


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
