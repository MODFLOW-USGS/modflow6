"""
Test the IST Package with a one cell model.  Check to make
Sure that sorption is working properly for the mobile
and immobile domains.

"""

import os

import numpy as np
import pytest

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = ["ist02"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# specify input parameters
nlay, nrow, ncol = 1, 1, 1
rhob_m = 1600. # kg/m^3 bulk density of the mobile domain
rhob_im = 1300. # kg/m^3 bulk density of the immobile domain
fhat_m = 0.75  # volume fraction of domain that is mobile
fhat_im = 1 - fhat_m # volume fraction of domain that is immobile
phi_m = 0.25 # volume of mobile domain pores per volume of mobile domain
phi_im = 0.45 # volume of immobile domain pores per volume of immobile domain
distribution_coefficient = 0.0001 # m^3 / kg
zeta_im = 1.0 # mass transfer coefficient

# calculated parameters
rhob = fhat_m * rhob_m + fhat_im * rhob_im  # average bulk density for all material
f_im = fhat_im * rhob_im / rhob  # solid mass fraction of immobile domain
f_m = 1 - f_im  # solid mass fraction of mobile domain
theta_m = fhat_m * phi_m  # volume of mobile domain pores per total volume
theta_im = fhat_im * phi_im # volume of immobile domain pores per total volume


def build_model(idx, dir):

    perlen = [
        1.0,
    ]
    nper = len(perlen)
    nstp = [1]
    tsmult = [1.0]
    delr = 10.0
    delc = 10.0
    top = 10.0
    botm = [0.0]
    strt = 10.
    hk = 1.0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 0.97

    tdis_rc = []
    for id in range(nper):
        tdis_rc.append((perlen[id], nstp[id], tsmult[id]))

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
    gwfname = "gwf_" + name
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
    )

    # create iterative model solution and register the gwf model with it
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        relaxation_factor=relax,
        filename=f"{gwfname}.ims",
    )
    sim.register_ims_package(imsgwf, [gwf.name])

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
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=False, icelltype=0, k=hk,
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.bud",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname, save_flows=True)

    # create iterative model solution and register the gwt model with it
    imsgwt = flopy.mf6.ModflowIms(
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
        filename=f"{gwtname}.ims",
    )
    sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0)

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(
        gwt, 
        sorption='linear',
        bulk_density=rhob, 
        porosity=theta_m, 
        distcoef=distribution_coefficient, 
    )

    # immobile storage and transfer
    cim_filerecord = f"{gwtname}.ist.ucn"
    ist = flopy.mf6.ModflowGwtist(
        gwt,
        save_flows=True,
        cim_filerecord=cim_filerecord,
        cim=0.0,
        sorption=True,
        bulk_density=rhob,  # todo: unclear why this is needed
        distcoef=distribution_coefficient,  # todo: okay to have different distribution coefficient for immobile domain?
        thetaim=theta_im,
        zetaim=zeta_im,
        fim=f_im,
    )

    # mass loading source
    srcdict = {0: [[(0, 0, 0), 1.0]]}
    src = flopy.mf6.ModflowGwtsrc(
        gwt, stress_period_data=srcdict, save_flows=False, pname="SRC-1"
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        concentration_filerecord=f"{gwtname}.ucn",
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename=f"{name}.gwfgwt",
    )

    return sim, None


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name
    gwfname = "gwf_" + name

    # head
    fpth = os.path.join(sim.simpath, f"{gwfname}.hds")
    try:
        hobj = flopy.utils.HeadFile(fpth, precision="double")
        head = hobj.get_alldata().flatten()
    except:
        assert False, f'could not load data from "{fpth}"'

    # mobile concentration
    fpth = os.path.join(sim.simpath, f"{gwtname}.ucn")
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        conc = cobj.get_alldata().flatten()
    except:
        assert False, f'could not load data from "{fpth}"'

    # immobile concentration
    fpth = os.path.join(sim.simpath, f"{gwtname}.ist.ucn")
    try:
        cobj = flopy.utils.HeadFile(fpth, precision="double", text="CIM")
        cim = cobj.get_alldata().flatten()
    except:
        assert False, f'could not load data from "{fpth}"'

    # budget
    fpth = os.path.join(sim.simpath, f"{gwtname}.cbc")
    try:
        bobj = flopy.utils.CellBudgetFile(fpth, precision="double")
        print(bobj.get_unique_record_names())
        immrate = bobj.get_data(text="IMMOBILE DOMAIN")
    except:
        assert False, f'could not load data from "{fpth}"'

    times = cobj.get_times()
    for i, t in enumerate(times):
        rate_sim = immrate[i]["q"][0]
        saturation = 1.0
        volume = 10.0 * 10.0 * 10.0
        rate_calc = (
            (cim[i] - conc[i]) * zeta_im * saturation * volume
        )
        print(t, conc[i], cim[i], rate_sim, rate_calc)
        msg = f"Rate: {rate_sim} /= {rate_calc} for time {t}"
        assert np.allclose(rate_sim, rate_calc), msg

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
