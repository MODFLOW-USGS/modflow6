# This autotest is based on the MOC3D problem 1 autotest except that it
# tests the zero order decay for a simple one-dimensional flow problem.
# The test ensures that concentrations do not go below zero (they do go
# slightly negative but, it does ensure that the decay rate shuts off as
# where concentrations are zero.

import os
import sys
import numpy as np

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

ex = [
    "moc3d01zoda",
    "moc3d01zodb",
]
diffc = [0, 0]
alphal = [0.1, 0.1]
retardation = [None, 40]
perlens = 4 * [120.0] + 3 * [240.0] + [120.0]
decay = [0.01, 0.01]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"


def get_model(idx, dir):
    nlay, nrow, ncol = 1, 122, 1
    nper = 1
    perlen = perlens[idx]  # [120.]
    perlen = [perlen]
    nstp = [240]
    tsmult = [1.0]
    steady = [True]
    delr = 0.1
    delc = 0.1
    top = 1.0
    botm = [0.0]
    strt = 1.0
    hnoflo = 1e30
    hdry = -1e30
    hk = 0.01
    laytyp = 0
    # ss = 0.
    # sy = 0.1

    nouter, ninner = 200, 300
    hclose, rclose, relax = 1e-8, 1e-6, 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        #continue_=True,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwfname = "gwf_" + name
    gwf = flopy.mf6.MFModel(
        sim,
        model_type="gwf6",
        modelname=gwfname,
        model_nam_file="{}.nam".format(gwfname),
    )

    # create iterative model solution and register the gwf model with it
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
        idomain=np.ones((nlay, nrow, ncol), dtype=int),
        filename="{}.dis".format(gwfname),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(
        gwf, strt=strt, filename="{}.ic".format(gwfname)
    )

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        save_specific_discharge=True,
        icelltype=laytyp,
        k=hk,
        k33=hk,
    )
    # storage
    # sto = flopy.mf6.ModflowGwfsto(gwf, save_flows=False,
    #                              iconvert=laytyp[idx],
    #                              ss=ss[idx], sy=sy[idx],
    #                              steady_state={0: True, 2: True},
    #                              transient={1: True})

    # chd files
    c = {0: [[(0, 121, 0), 0.0000000]]}
    chd = flopy.mf6.ModflowGwfchd(
        gwf, stress_period_data=c, save_flows=False, pname="CHD-1"
    )

    # wel files
    w = {0: [[(0, 0, 0), 0.001, 1.0]]}
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        print_input=True,
        print_flows=True,
        stress_period_data=w,
        save_flows=False,
        auxiliary="CONCENTRATION",
        pname="WEL-1",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(gwfname),
        head_filerecord="{}.hds".format(gwfname),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
        model_nam_file="{}.nam".format(gwtname),
    )

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
        filename="{}.ims".format(gwtname),
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
        idomain=1,
        filename="{}.dis".format(gwtname),
    )

    # initial conditions
    strt = np.zeros((nlay, nrow, ncol))
    strt[0, 0, 0] = 0.0
    ic = flopy.mf6.ModflowGwtic(
        gwt, strt=strt, filename="{}.ic".format(gwtname)
    )

    # advection
    adv = flopy.mf6.ModflowGwtadv(
        gwt, scheme="tvd", filename="{}.adv".format(gwtname)
    )

    # dispersion
    dsp = flopy.mf6.ModflowGwtdsp(
        gwt,
        diffc=diffc[idx],
        alh=alphal[idx],
        alv=alphal[idx],
        ath1=0.0,
        atv=0.0,
        filename="{}.dsp".format(gwtname),
    )

    # constant concentration
    # cncs = {0: [[(0, 0, 0), 1.0]]}
    # cnc = flopy.mf6.ModflowGwtcnc(gwt, maxbound=len(cncs),
    #                              stress_period_data=cncs,
    #                              save_flows=False,
    #                              pname='CNC-1')

    # storage
    porosity = 0.1

    rtd = retardation[idx]
    sorption = None
    kd = None
    rhob = None
    if rtd is not None:
        rhob = 1.0
        kd = (rtd - 1.0) * porosity / rhob
        sorption = "linear"

    decay_rate = decay[idx]
    zero_order_decay = False
    if decay_rate is not None:
        zero_order_decay = True

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(
        gwt,
        porosity=porosity,
        zero_order_decay=zero_order_decay,
        decay=decay_rate,
        decay_sorbed=decay_rate,
        sorption=sorption,
        distcoef=kd,
        bulk_density=rhob,
    )

    # sources
    sourcerecarray = [("WEL-1", "AUX", "CONCENTRATION")]
    ssm = flopy.mf6.ModflowGwtssm(
        gwt, sources=sourcerecarray, filename="{}.ssm".format(gwtname)
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord="{}.cbc".format(gwtname),
        concentration_filerecord="{}.ucn".format(gwtname),
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
    )

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename="{}.gwfgwt".format(name),
    )

    return sim


def make_plot_ct(tssim, fname=None):
    """Concentration versus time plot"""
    import matplotlib.pyplot as plt

    fig = plt.figure(figsize=(6, 3))
    ax = fig.add_subplot(1, 1, 1)
    mec = ["red", "blue", "green"]
    iskip = 2
    tssim = tssim[::iskip]
    for i, l in enumerate(["x=0.05", "x=4.05", "x=11.05"]):
        ax.plot(
            tssim[:, 0],
            tssim[:, i + 1],
            marker="o",
            ls="none",
            mec=mec[i],
            mfc="none",
            markersize="4",
            label=l,
        )

    ax.set_xlabel("Time (seconds)")
    ax.set_ylabel("Normalized Concentration, dimensionless")
    plt.legend()

    if fname is not None:
        plt.savefig(fname, bbox_inches="tight")
    return


def make_plot_cd(cobj, fname=None):
    """Concentration versus time plot"""
    import matplotlib.pyplot as plt

    fig = plt.figure(figsize=(6, 3))
    ax = fig.add_subplot(1, 1, 1)
    delr = 0.1
    system_length = 12.0
    ncol = 122
    iskip = 1
    mec = ["red", "blue", "green"]
    x = np.linspace(0.5 * delr, system_length - 0.5 * delr, ncol)
    for i, t in enumerate([6.0, 60.0, 120.0]):
        conc = cobj.get_data(totim=t).flatten()
        ax.plot(
            x[::iskip],
            conc[::iskip],
            marker="o",
            ls="none",
            mec=mec[i],
            mfc="none",
            markersize="4",
            label="t={} s".format(t),
        )

    ax.set_xlabel("Distance (cm)")
    ax.set_ylabel("Normalized Concentration, dimensionless")
    plt.legend()

    if fname is not None:
        plt.savefig(fname, bbox_inches="tight")
    return


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name

    fpth = os.path.join(sim.simpath, "{}.ucn".format(gwtname))
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        station = [(0, 0, 0), (0, 40, 0), (0, 110, 0)]
        tssim = cobj.get_ts(station)
    except:
        assert False, 'could not load data from "{}"'.format(fpth)

    makeplot = False
    if makeplot:
        fname = "fig-ct.pdf"
        fname = os.path.join(exdirs[sim.idxsim], fname)
        make_plot_ct(tssim, fname)

        fname = "fig-cd.pdf"
        fname = os.path.join(exdirs[sim.idxsim], fname)
        make_plot_cd(cobj, fname)

    tssim = tssim[::10]

    # answer for case with decay and no sorption; taken from run that appeared
    # to have the correct answer.
    tsresa = [
        [5.00000000e-01, 2.80021420e-01, -6.27799728e-15, 1.43673140e-37],
        [5.50000000e00, 9.02564301e-01, -4.87485503e-13, -2.00947321e-28],
        [1.05000000e01, 9.67062210e-01, -1.05981417e-10, -2.48087545e-24],
        [1.55000000e01, 9.78915405e-01, -3.53821419e-09, -1.18486066e-21],
        [2.05000000e01, 9.81493335e-01, -2.99478587e-08, -9.33100014e-20],
        [2.55000000e01, 9.82102870e-01, -9.51070742e-08, -2.45893471e-18],
        [3.05000000e01, 9.82254646e-01, 8.60641012e-08, -3.77836341e-17],
        [3.55000000e01, 9.82293842e-01, 3.30654200e-02, -5.75101970e-16],
        [4.05000000e01, 9.82304244e-01, 1.57628457e-01, -2.05302102e-14],
        [4.55000000e01, 9.82307064e-01, 3.01251363e-01, -4.60722217e-13],
        [5.05000000e01, 9.82307841e-01, 4.17994177e-01, -6.37157398e-12],
        [5.55000000e01, 9.82308058e-01, 4.95717338e-01, -5.73893847e-11],
        [6.05000000e01, 9.82308119e-01, 5.40830243e-01, -3.51020106e-10],
        [6.55000000e01, 9.82308137e-01, 5.64486282e-01, -1.51583615e-09],
        [7.05000000e01, 9.82308142e-01, 5.75955359e-01, -4.83651509e-09],
        [7.55000000e01, 9.82308143e-01, 5.81180311e-01, -1.19745768e-08],
        [8.05000000e01, 9.82308144e-01, 5.83443818e-01, -2.40842376e-08],
        [8.55000000e01, 9.82308144e-01, 5.84384825e-01, -4.09015146e-08],
        [9.05000000e01, 9.82308144e-01, 5.84762931e-01, -6.04279172e-08],
        [9.55000000e01, 9.82308144e-01, 5.84910599e-01, -7.98560873e-08],
        [1.00500000e02, 9.82308144e-01, 5.84966906e-01, -9.94479089e-08],
        [1.05500000e02, 9.82308144e-01, 5.84987944e-01, -1.22122044e-07],
        [1.10500000e02, 9.82308144e-01, 5.84995668e-01, -1.42873487e-07],
        [1.15500000e02, 9.82308144e-01, 5.84998462e-01, -1.59338732e-07],
    ]

    # answer for case with decay and sorption
    tsresb = [
        [5.00000000e-001, 1.08536585e-002, 1.50230289e-065, 7.03720789e-179],
        [5.50000000e000, 1.05972468e-001, -1.20770394e-056, -1.16040412e-164],
        [1.05000000e001, 1.81719287e-001, -1.73997465e-052, -2.69101712e-156],
        [1.55000000e001, 2.43729942e-001, -1.86616329e-049, -1.16363650e-149],
        [2.05000000e001, 2.95729413e-001, -7.29308335e-047, -3.99240889e-144],
        [2.55000000e001, 3.40178152e-001, -1.28823958e-044, -2.35667043e-139],
        [3.05000000e001, 3.78744937e-001, -1.07712892e-042, -3.79220669e-135],
        [3.55000000e001, 4.12607082e-001, -4.92494757e-041, -2.24709157e-131],
        [4.05000000e001, 4.42623555e-001, -1.40845582e-039, -6.05974088e-128],
        [4.55000000e001, 4.69439605e-001, -2.78724557e-038, -8.68988128e-125],
        [5.05000000e001, 4.93551649e-001, -4.10767481e-037, -7.43207001e-122],
        [5.55000000e001, 5.15348967e-001, -4.75649699e-036, -4.11388640e-119],
        [6.05000000e001, 5.35142116e-001, -4.50278413e-035, -1.55946013e-116],
        [6.55000000e001, 5.53183571e-001, -3.59069429e-034, -4.21269863e-114],
        [7.05000000e001, 5.69682313e-001, -2.46839024e-033, -8.36120511e-112],
        [7.55000000e001, 5.84813940e-001, -1.48978956e-032, -1.25121687e-109],
        [8.05000000e001, 5.98727767e-001, -8.01195046e-032, -1.44498742e-107],
        [8.55000000e001, 6.11551927e-001, -3.88652458e-031, -1.31567108e-105],
        [9.05000000e001, 6.23397138e-001, -1.71810925e-030, -9.63155190e-104],
        [9.55000000e001, 6.34359576e-001, -6.98234537e-030, -5.77117544e-102],
        [1.00500000e002, 6.44523151e-001, -2.62836345e-029, -2.87651068e-100],
        [1.05500000e002, 6.53961371e-001, -9.22475734e-029, -1.21009541e-098],
        [1.10500000e002, 6.62738943e-001, -3.03610362e-028, -4.35314284e-097],
        [1.15500000e002, 6.70913131e-001, -9.41863049e-028, -1.35492374e-095],
    ]

    tsresa = np.array(tsresa)
    tsresb = np.array(tsresb)
    tsreslist = [tsresa, tsresb]
    tsres = tsreslist[sim.idxsim]
    if tsres is not None:
        assert np.allclose(
            tsres, tssim
        ), "simulated concentrations do not match with known solution."

    return


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_transport, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
