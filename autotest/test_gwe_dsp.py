# ## Test problem for GWE
#
# One-Dimensional Transport in a Uniform Flow Field.
# The purpose of this script is to test the new heat transport model developed
# for MODFLOW 6.  To that end, this problem uses the setup of the first MT3DMS
# test problem but adapts it for heat. MODFLOW 6 is setup using the new GWE 
# model with input parameters entered in their native units.  The equivalent
# values are calculated for "tricking" MT3DMS into heat transport.  
#
# It may be possible to find a 1D heat transport analytical solution in the 
# future.

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

import targets

exe_name_mf = targets.target_dict["mf2005s"]
exe_name_mt = targets.target_dict["mt3dms"]
exe_name_mf6 = targets.target_dict["mf6"]

# Base simulation and model name and workspace

viscosity_on = [False]
ex = ["dsp01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# Model units

length_units = "meters"
time_units = "days"

# Table MODFLOW 6 GWE comparison to MT3DMS

nper = 1  # Number of periods
nlay = 1  # Number of layers
ncol = 101  # Number of columns
nrow = 1  # Number of rows
delr = 10.0  # Column width ($m$)
delc = 1.0  # Row width ($m$)
top = 0.0  # Top of the model ($m$)
botm = -1.0  # Layer bottom elevations ($m$)
prsity = 0.25  # Porosity
perlen = 2000  # Simulation time ($days$)
k11 = 1.0  # Horizontal hydraulic conductivity ($m/d$)

# Set some static model parameter values

k33 = k11  # Vertical hydraulic conductivity ($m/d$)
laytyp = 1
nstp = 100.0
dt0 = perlen / nstp
Lx = (ncol - 1) * delr
v = 0.24
q = v * prsity
h1 = q * Lx
strt = np.zeros((nlay, nrow, ncol), dtype=float)
strt[0, 0, 0] = h1  # Starting head ($m$)
l = 1000.0  # Needed for plots
icelltype = 1  # Cell conversion type
ibound = np.ones((nlay, nrow, ncol), dtype=int)
ibound[0, 0, 0] = -1
ibound[0, 0, -1] = -1

# Set some static transport related model parameter values

mixelm = 0  # FD
rhob = 1110.0
sp2 = 0.0  # read, but not used in this problem
kd = 1.8168E-4
strt_temp = np.zeros((nlay, nrow, ncol), dtype=float)
dispersivity = 1.0
dmcoef = 3.2519E-7  # Molecular diffusion coefficient

# Set solver parameter values (and related)
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-6, 1e-6, 1.0
ttsmult = 1.0
dceps = 1.0e-5  # HMOC parameters in case they are invoked
nplane = 1  # HMOC
npl = 0  # HMOC
nph = 4  # HMOC
npmin = 0  # HMOC
npmax = 8  # HMOC
nlsink = nplane  # HMOC
npsink = nph  # HMOC

# Static temporal data used by TDIS file

tdis_rc = []
tdis_rc.append((perlen, nstp, 1.0))

# ### Create MODFLOW 6 GWE MT3DMS Example 1 Boundary Conditions
#
# Constant head cells are specified on both ends of the model

chdspd = [[(0, 0, 0), h1], [(0, 0, ncol - 1), 0.0]]
c0 = 40.0
ctpspd = [[(0, 0, 0), c0]]


#
# MF2K5/MT3DMS and MODFLOW 6 (sim) flopy objects returned if building the model
#

def build_mfmt_models(idx, dir):
    # Base MF2K5/MT3DMS runs
    ws = dir
    name = ex[idx]

    mt3d_ws = os.path.join(ws, name, "mt3d")
    modelname_mf = "p01-mf"

    # Instantiate the MODFLOW model
    mf = flopy.modflow.Modflow(
        modelname=modelname_mf, model_ws=mt3d_ws, exe_name=exe_name_mf
    )

    # Instantiate discretization package
    # units: itmuni=4 (days), lenuni=2 (m)
    flopy.modflow.ModflowDis(
        mf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        nstp=nstp,
        botm=botm,
        perlen=perlen,
        itmuni=4,
        lenuni=2,
    )

    # Instantiate basic package
    flopy.modflow.ModflowBas(mf, ibound=ibound, strt=strt)

    # Instantiate layer property flow package
    flopy.modflow.ModflowLpf(mf, hk=k11, laytyp=laytyp)

    # Instantiate solver package
    flopy.modflow.ModflowPcg(mf)

    # Instantiate link mass transport package (for writing linker file)
    flopy.modflow.ModflowLmt(mf)

    # Write and run the simulation to create the linker file
    mf.write_input()
    mf.run_model(silent=False)

    # Transport
    modelname_mt = "p01-mt"
    mt = flopy.mt3d.Mt3dms(
        modelname=modelname_mt,
        model_ws=mt3d_ws,
        exe_name=exe_name_mt,
        modflowmodel=mf,
    )

    icbund = np.ones((nlay, nrow, ncol), dtype=int)
    icbund[0, 0, 0] = -1
    strt_temp = np.zeros((nlay, nrow, ncol), dtype=float)
    strt_temp[0, 0, 0] = c0
    flopy.mt3d.Mt3dBtn(
        mt,
        laycon=laytyp,
        icbund=icbund,
        prsity=prsity,
        sconc=strt_temp,
        dt0=dt0,
        ifmtcn=1,
    )

    # Instatiate the advection package
    flopy.mt3d.Mt3dAdv(
        mt,
        mixelm=mixelm,
        dceps=dceps,
        nplane=nplane,
        npl=npl,
        nph=nph,
        npmin=npmin,
        npmax=npmax,
        nlsink=nlsink,
        npsink=npsink,
        percel=0.5,
    )

    # Instantiate the dispersion package
    flopy.mt3d.Mt3dDsp(mt, al=dispersivity, dmcoef=dmcoef)

    # Set reactive variables and instantiate chemical reaction package
    isothm = 1
    flopy.mt3d.Mt3dRct(
        mt,
        isothm=isothm,
        ireact=0,
        igetsc=0,
        rhob=rhob,
        sp1=kd
    )

    # Instantiate the source/sink mixing package
    flopy.mt3d.Mt3dSsm(mt)

    # Instantiate the GCG solver in MT3DMS
    flopy.mt3d.Mt3dGcg(mt, mxiter=10)

    mt.write_input()
    fname = os.path.join(mt3d_ws, "MT3D001.UCN")
    if os.path.isfile(fname):
        os.remove(fname)
    mt.run_model(silent=False)

    ucnobj = flopy.utils.UcnFile(fname)
    times = ucnobj.get_times()
    conc = ucnobj.get_alldata()

    return conc, times


def build_mf6_models(idx, dir):
    # Base MF6 GWE model type
    ws = dir
    name = ex[idx]

    print("Building MF6 model...()".format(name))

    # generate names for each model
    gwfname = "gwf-" + name
    gwename = "gwe-" + name

    sim_ws = os.path.join(ws, name)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=ws, exe_name=exe_name_mf6, version="mf6"
    )

    # Instantiating MODFLOW 6 time discretization
    flopy.mf6.ModflowTdis(
        sim, nper=nper, perioddata=tdis_rc, time_units=time_units
    )

    # Instantiating MODFLOW 6 groundwater flow model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        save_flows=True,
        model_nam_file="{}.nam".format(gwfname),
    )

    # Instantiating MODFLOW 6 solver for flow model
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
    sim.register_ims_package(imsgwf, [gwfname])

    # Instantiating MODFLOW 6 discretization package
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
        idomain=np.ones((nlay, nrow, ncol), dtype=int),
        filename="{}.dis".format(gwfname),
    )

    # Instantiating MODFLOW 6 node-property flow package
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        icelltype=icelltype,
        k=k11,
        k33=k33,
        save_specific_discharge=True,
        filename="{}.npf".format(gwfname),
    )

    # Instantiating MODFLOW 6 initial conditions package for flow model
    flopy.mf6.ModflowGwfic(
        gwf, strt=strt, filename="{}.ic".format(gwfname)
    )

    # Instantiating VSC
    if viscosity_on[idx]:
        # Instantiate viscosity (VSC) package
        vsc_filerecord = "{}.vsc.bin".format(gwfname)
        vsc_pd = [(0, 0.0, 20.0, gwename, "temperature")]
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

    # Instantiating MODFLOW 6 constant head package
    flopy.mf6.ModflowGwfchd(
        gwf,
        maxbound=len(chdspd),
        stress_period_data=chdspd,
        save_flows=False,
        pname="CHD-1",
        filename="{}.chd".format(gwfname),
    )

    # Instantiating MODFLOW 6 output control package for flow model
    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord="{}.hds".format(gwfname),
        budget_filerecord="{}.cbc".format(gwfname),
        headprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # Instantiating MODFLOW 6 groundwater transport package
    gwe = flopy.mf6.MFModel(
        sim,
        model_type="gwe6",
        modelname=gwename,
        model_nam_file="{}.nam".format(gwename),
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
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=1,
        filename="{}.dis".format(gwename),
    )

    # Instantiating MODFLOW 6 transport initial concentrations
    flopy.mf6.ModflowGweic(
        gwe, strt=strt_temp, filename="{}.ic".format(gwename)
    )

    # Instantiating MODFLOW 6 transport advection package
    if mixelm == 0:
        scheme = "UPSTREAM"
    elif mixelm == -1:
        scheme = "TVD"
    else:
        raise Exception()
    flopy.mf6.ModflowGweadv(
        gwe, scheme=scheme, filename="{}.adv".format(gwename)
    )

    # Instantiating MODFLOW 6 transport dispersion package
    if dispersivity != 0:
        flopy.mf6.ModflowGwedsp(
            gwe,
            xt3d_off=True,
            alh=dispersivity,
            ath1=dispersivity,
            ktw=0.5918,
            kts=0.2700,
            filename="{}.dsp".format(gwename),
        )

    # Instantiating MODFLOW 6 transport mass storage package (formerly "reaction" package in MT3DMS)
    flopy.mf6.ModflowGwemst(
        gwe,
        porosity=prsity,
        cpw=4183.0,
        cps=760.0,
        rhow=1000.0,
        rhos=1500.0,
        filename="{}.mst".format(gwename),
    )

    # Instantiating MODFLOW 6 transport constant concentration package
    flopy.mf6.ModflowGwetmp(
        gwe,
        maxbound=len(ctpspd),
        stress_period_data=ctpspd,
        save_flows=False,
        pname="TMP-1",
        filename="{}.tmp".format(gwename),
    )

    # Instantiating MODFLOW 6 transport source-sink mixing package
    flopy.mf6.ModflowGwessm(
        gwe, sources=[[]], filename="{}.ssm".format(gwename)
    )

    # Instantiate MODFLOW 6 heat transport output control package
    flopy.mf6.ModflowGweoc(
        gwe,
        budget_filerecord="{}.cbc".format(gwename),
        temperature_filerecord="{}.ucn".format(gwename),
        temperatureprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("TEMPERATURE", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("TEMPERATURE", "LAST"), ("BUDGET", "LAST")],
    )

    # Instantiating MODFLOW 6 flow-transport exchange mechanism
    flopy.mf6.ModflowGwfgwe(
        sim,
        exgtype="GWF6-GWE6",
        exgmnamea=gwfname,
        exgmnameb=gwename,
        filename="{}.gwfgwe".format(name),
    )

    # Grab output
    sim.write_simulation()
    fname = os.path.join(ws, gwename + ".ucn")
    if os.path.isfile(fname):
        os.remove(fname)
    success, buff = sim.run_simulation(silent=False, report=True)
    if not success:
        print(buff)

    # load temperatures
    ucnobj = flopy.utils.HeadFile(fname, precision="double", text="TEMPERATURE")
    times = ucnobj.get_times()
    conc = ucnobj.get_alldata()

    return conc, times


# Function to write model files
def write_model(mf2k5, mt3d, sim, silent=True):
    if config.writeModel:
        mf2k5.write_input()
        mt3d.write_input()
        sim.write_simulation(silent=silent)


# Function to ensure GWE model is working properly
def eval_results(mt3d, mf6):
    print("evaluating results...")

    # read transport results from model
    mt3d_out_path = mt3d.model_ws
    mf6_out_path = mf6.simulation_data.mfpath.get_sim_path()
    mf6.simulation_data.mfpath.get_sim_path()

    # Get the MT3DMS concentration output
    fname_mt3d = os.path.join(mt3d_out_path, "MT3D001.UCN")
    ucnobj_mt3d = flopy.utils.UcnFile(fname_mt3d)
    conc_mt3d = ucnobj_mt3d.get_alldata()

    # Get the MF6 concentration output
    gwt = mf6.get_model(list(mf6.model_names)[1])
    #ucnobj_mf6 = gwt.output.temperature()
    ucnobj_mf6 = gwt.output.concentration()
    conc_mf6 = ucnobj_mf6.get_alldata()


def eval_results(sim):
    print("evaluating results...")

    # read transport results from GWE model
    name = ex[sim.idxsim]
    gwfname = "gwf-" + name

    fname = gwfname + ".bud"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    budobj = flopy.utils.CellBudgetFile(fname, precision="double")
    outbud = budobj.get_data(text="             GHB")


def test_gwe_dsp01():

    # run the test model
    idx = 0
    dir = exdirs[idx]

    mt3d_conc, mt3d_times = build_mfmt_models(idx, dir)
    gwe_temp, gwe_times = build_mf6_models(idx, dir)

    msg = f"gwe temperatures do not equal mt3dms concentrations"
    assert np.allclose(gwe_temp, mt3d_conc, atol=0.41159), msg


if __name__ == "__main__":
    # ### Heat Transport in 1-dimension
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    test_gwe_dsp01()
