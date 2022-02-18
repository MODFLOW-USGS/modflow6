# Second problem described by Prudic et al (2004)
# This problem involves transport through an aquifers, lakes and streams.
# It requires the use of the Water Mover Package to send water from a stream,
# into a lake, and then back into another stream. Solute is also transport
# through the system.

import os
import pytest
import sys
import numpy as np

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

data_ws = "./data/prudic2004test2gwtgwt/"
ex = ["prudic2004t2gwtgwt"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

gwfnames = ["flow1", "flow2"]
gwtnames = ["transport1", "transport2"]

# grid information
nlay = 8
nrow = 36
ncol = 23
delr = 405.665
delc = 403.717
top = 100.0
fname = os.path.join(data_ws, "bot1.dat")
bot0 = np.loadtxt(fname)
botm = [bot0] + [bot0 - (15.0 * k) for k in range(1, nlay)]

# set functionality for flow
sto_on = False
rch_on = True
sfr_on = True
lake_on = True
within_model_mvr_on = True
across_model_mvr_on = True

# set functionality for transport
transport_on = True
lkt_on = True and lake_on
sft_on = True and sfr_on
within_model_mvt_on = True and within_model_mvr_on
across_model_mvt_on = True and across_model_mvr_on

# setup idomain
fname = os.path.join(data_ws, "idomain1.dat")
idomain0 = np.loadtxt(fname, dtype=int)
idomain = nlay * [idomain0]
idomain = np.array(idomain)

fname = os.path.join(data_ws, "lakibd.dat")
lakibd = np.loadtxt(fname, dtype=int)


def build_model(idx, ws):

    name = "mf6sim"
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        continue_=False,
    )
    tdis_rc = [(1.0, 1, 1.0), (365.25 * 25, 25, 1.0)]
    nper = len(tdis_rc)
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="DAYS",
        nper=nper,
        perioddata=tdis_rc,
        filename="mfsim.tdis",
    )

    # Flow solver
    hclose = 0.001
    rclose = 0.1
    nouter = 1000
    ninner = 100
    relax = 0.99
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
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
        filename="flow.ims",
    )

    # Transport solver
    imsgwt = None
    if transport_on:
        imsgwt = flopy.mf6.ModflowIms(
            sim,
            print_option="ALL",
            outer_dvclose=hclose,
            outer_maximum=nouter,
            inner_maximum=ninner,
            under_relaxation="DBD",
            under_relaxation_theta=0.7,
            inner_dvclose=hclose,
            rcloserecord=rclose,
            linear_acceleration="BICGSTAB",
            scaling_method="NONE",
            reordering_method="NONE",
            relaxation_factor=relax,
            filename="transport.ims",
        )

    # imodelcombo is set to have gwf1/gwt1 apply to a value of 1
    # and gwf2/gwt2 apply to a value of 2
    imodelcombo = np.ones((nlay, nrow, ncol), dtype=int)
    imodelcombo[:, 16:, :] = 2

    # north side
    icombo = 1
    sim = build_gwfgwt_combo(
        sim,
        gwfnames[icombo - 1],
        gwtnames[icombo - 1],
        idomain,
        imodelcombo,
        icombo,
        imsgwf,
        imsgwt,
    )

    # south side
    icombo = 2
    sim = build_gwfgwt_combo(
        sim,
        gwfnames[icombo - 1],
        gwtnames[icombo - 1],
        idomain,
        imodelcombo,
        icombo,
        imsgwf,
        imsgwt,
    )

    # add a gwf-gwf exchange
    angldegx = 270.0
    cdist = delc
    gwfgwf_data = []
    for k in range(nlay):
        for j in range(ncol):
            connection = [
                (k, 16 - 1, j),
                (k, 17 - 1, j),
                1,
                delc / 2.0,
                delc / 2,
                delr,
                angldegx,
                cdist,
            ]
            add_connection = True
            if idomain[k, 16 - 1, j] == 0:
                add_connection = False
            if idomain[k, 17 - 1, j] == 0:
                add_connection = False
            if lake_on:
                if k == 0 and lakibd[16 - 1, j] > 0:
                    add_connection = False
                if k == 0 and lakibd[17 - 1, j] > 0:
                    add_connection = False
            if add_connection:
                gwfgwf_data.append(connection)

    # GWF-GWF
    mvr_filerecord = None
    if across_model_mvr_on:
        mvr_filerecord = f"{name}.gwfgwf.mvr"
    gwfgwf = flopy.mf6.ModflowGwfgwf(
        sim,
        exgtype="GWF6-GWF6",
        nexg=len(gwfgwf_data),
        exgmnamea=gwfnames[0],  # north
        exgmnameb=gwfnames[1],  # south
        exchangedata=gwfgwf_data,
        auxiliary=["ANGLDEGX", "CDIST"],
        dev_interfacemodel_on=False,
        mvr_filerecord=mvr_filerecord,
    )

    # simulation GWF-GWF Mover
    if across_model_mvr_on:
        maxmvr, maxpackages = 1, 2
        mvrpack_sim = [["flow1", "lak-1"], ["flow2", "sfr-2"]]
        mvrspd = [["flow1", "lak-1", 0, "flow2", "sfr-2", 0, "FACTOR", 1.00]]
        mvr = flopy.mf6.ModflowMvr(
            sim,
            modelnames=True,
            maxmvr=maxmvr,
            print_flows=True,
            maxpackages=maxpackages,
            packages=mvrpack_sim,
            perioddata=mvrspd,
            filename=f"{name}.gwfgwf.mvr",
        )

    # GWT-GWT
    mvt_filerecord = None
    if across_model_mvt_on:
        mvt_filerecord = f"{name}.gwtgwt.mvt"
    gwtgwt = flopy.mf6.ModflowGwtgwt(
        sim,
        exgtype="GWT6-GWT6",
        nexg=len(gwfgwf_data),
        exgmnamea=gwtnames[0],  # north
        exgmnameb=gwtnames[1],  # south
        gwfmodelname1=gwfnames[0],
        gwfmodelname2=gwfnames[1],
        exchangedata=gwfgwf_data,
        auxiliary=["ANGLDEGX", "CDIST"],
        mvt_filerecord=mvt_filerecord,
        #dev_interfacemodel_on=False,
    )

    # simulation GWT-GWT Mover
    if across_model_mvt_on:
        mvt = flopy.mf6.modflow.ModflowGwtmvt(sim, filename=f"{name}.gwtgwt.mvt")

    regression = None
    return sim, regression

def sfr_packagedata_to_list(fname, gwf, boundnames=False, convert_to_zero_base=True):
    dt = flopy.mf6.ModflowGwfsfr.packagedata.dtype(gwf, cellid_expanded=True,
                                               boundnames=boundnames, timeseries=False)
    dt[10] = ("man", float)
    if boundnames:
        dt[14] = ("boundname", 'S40')
    ra = np.genfromtxt(fname, dtype=dt)
    if convert_to_zero_base:
        ra["rno"] -= 1
        ra["layer"] -= 1
        ra["row"] -= 1
        ra["column"] -= 1

    sfrpdlist = []
    for rec in ra:
        t = list(rec)
        l = [t[0], (t[1], t[2], t[3])] + t[4:]
        sfrpdlist.append(l)

    return sfrpdlist

def sfr_connectiondata_to_list(fname, convert_to_zero_base=True):
    with open(fname) as f:
        cd_list = [[int(i) for i in s.strip().split()] for s in f.readlines()]
    if convert_to_zero_base:
        cd_list = [[np.sign(irch) * (abs(irch) - 1) for irch in l] for l in cd_list]
    return cd_list


def build_gwfgwt_combo(sim, gwfname, gwtname, idomain, imodelcombo, icombo, imsgwf, imsgwt):

    # number of time steps for period 2 are reduced from 12 * 25 to 25 in
    # order to speed up this autotest

    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname)
    sim.register_ims_package(imsgwf, [gwf.name])

    # set idomain to zero where this model combo is not active
    idxmask = imodelcombo != icombo
    idomain = idomain.copy()
    idomain[idxmask] = 0

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )
    idomain = dis.idomain.array

    ic = flopy.mf6.ModflowGwfic(gwf, strt=50.0)

    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=[1] + 7 * [0],
        k=250.0,
        k33=125.0,
    )

    if sto_on:
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=True,
            iconvert=[1] + 7 * [0],
            ss=1.0e-5,
            sy=0.3,
            steady_state={0: True},
            transient={1: False},
        )

    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.bud".format(gwfname),
        head_filerecord="{}.hds".format(gwfname),
        headprintrecord=[
            ("COLUMNS", ncol, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    if rch_on:
        rch = flopy.mf6.ModflowGwfrcha(
            gwf, recharge={0: 4.79e-3}, pname="RCH-1"
        )

    if icombo == 1:
        fname = "chd_north.dat"
    else:
        fname = "chd_south.dat"
    chdlist = []
    fname = os.path.join(data_ws, fname)
    print(f"Setting CHD information from: {fname}")
    for line in open(fname, "r").readlines():
        ll = line.strip().split()
        if len(ll) == 4:
            k, i, j, hd = ll
            chdlist.append([int(k) - 1, int(i) - 1, int(j) - 1, float(hd)])
    chd = flopy.mf6.ModflowGwfchd(
        gwf, stress_period_data=chdlist, pname="CHD-1"
    )

    if sfr_on:
        if icombo == 1:
            isfrseglist = [1]
        else:
            isfrseglist = [2, 3, 4]
        for isfrseg in isfrseglist:
            sfrperioddata = None
            if isfrseg == 1:
                sfrperioddata = {0: [[0, "inflow", 86400], ]}
            if isfrseg == 3:
                sfrperioddata = {0: [[0, "inflow", 8640.0], ]}
            fname = os.path.join(data_ws, f"sfr-packdata-{isfrseg}.dat")
            sfrpd = sfr_packagedata_to_list(fname, gwf)
            nreaches = len(sfrpd)
            fname = os.path.join(data_ws, f"sfr-conndata-{isfrseg}.dat")
            sfrcd = sfr_connectiondata_to_list(fname)
            print(f"Setting nreaches to {nreaches}")
            sfr = flopy.mf6.ModflowGwfsfr(
                gwf,
                print_stage=True,
                print_flows=True,
                stage_filerecord=gwfname + f".sfr{isfrseg}.bin",
                budget_filerecord=gwfname + f".sfr{isfrseg}.bud",
                mover=True,
                pname=f"SFR-{isfrseg}",
                filename=f"{gwfname}.sfr{isfrseg}",
                unit_conversion=128390.00,
                boundnames=False,
                nreaches=nreaches,
                packagedata=sfrpd,
                connectiondata=sfrcd,
                perioddata=sfrperioddata,
            )

    lakeibd_list = [1]
    if icombo == 2:
        lakeibd_list = [2]
    lakeconnectiondata = []
    nlakecon = [0]
    lak_leakance = 1.0
    for i in range(nrow):
        for j in range(ncol):
            if lakibd[i, j] not in lakeibd_list:
                continue
            else:

                ilak = lakibd[i, j] - 1
                ilak = 0
                # back
                if i > 0:
                    if lakibd[i - 1, j] == 0 and idomain[0, i - 1, j] != 0:
                        h = [
                            ilak,
                            nlakecon[ilak],
                            (0, i - 1, j),
                            "horizontal",
                            lak_leakance,
                            0.0,
                            0.0,
                            delc / 2.0,
                            delr,
                        ]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # left
                if j > 0:
                    if lakibd[i, j - 1] != 0 and idomain[0, i, j - 1] == 0:
                        h = [
                            ilak,
                            nlakecon[ilak],
                            (0, i, j - 1),
                            "horizontal",
                            lak_leakance,
                            0.0,
                            0.0,
                            delr / 2.0,
                            delc,
                        ]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # right
                if j < ncol - 1:
                    if lakibd[i, j + 1] == 0 and idomain[0, i, j + 1] != 0:
                        h = [
                            ilak,
                            nlakecon[ilak],
                            (0, i, j + 1),
                            "horizontal",
                            lak_leakance,
                            0.0,
                            0.0,
                            delr / 2.0,
                            delc,
                        ]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # front
                if i < nrow - 1:
                    if lakibd[i + 1, j] == 0 and idomain[0, i + 1, j] != 0:
                        h = [
                            ilak,
                            nlakecon[ilak],
                            (0, i + 1, j),
                            "horizontal",
                            lak_leakance,
                            0.0,
                            0.0,
                            delc / 2.0,
                            delr,
                        ]
                        nlakecon[ilak] += 1
                        lakeconnectiondata.append(h)
                # vertical
                v = [
                    ilak,
                    nlakecon[ilak],
                    (1, i, j),
                    "vertical",
                    lak_leakance,
                    0.0,
                    0.0,
                    0.0,
                    0.0,
                ]
                nlakecon[ilak] += 1
                lakeconnectiondata.append(v)

    outlets = None
    noutlets = 0
    if icombo == 1:
        lakpackagedata = [[0, 44.0, nlakecon[0], "lake1"]]
        # <outletno> <lakein> <lakeout> <couttype> <invert> <width> <rough> <slope>
        outlets = [[0, 0, -1, "MANNING", 44.5, 5.000000, 0.03, 0.2187500e-02]]
        noutlets = 1
    elif icombo == 2:
        lakpackagedata = [[0, 35.2, nlakecon[0], "lake2"]]
    else:
        raise Exception('brokin')

    if lake_on:
        i, j = np.where(lakibd > 0)
        idomain[0, i, j] = 0
        gwf.dis.idomain.set_data(idomain[0], layer=0, multiplier=[1])

        lak = flopy.mf6.ModflowGwflak(
            gwf,
            time_conversion=86400.000,
            print_stage=True,
            print_flows=True,
            stage_filerecord=gwfname + ".lak.bin",
            budget_filerecord=gwfname + ".lak.bud",
            mover=True,
            pname="LAK-1",
            boundnames=True,
            nlakes=len(lakpackagedata),
            noutlets=noutlets,
            outlets=outlets,
            packagedata=lakpackagedata,
            connectiondata=lakeconnectiondata,
        )

    if within_model_mvr_on:

        if icombo == 1:
            maxmvr, maxpackages = 1, 2
            mvrpack = [["SFR-1"], ["LAK-1"]]
            mvrperioddata = [
                ["SFR-1", 5, "LAK-1", 0, "FACTOR", 1.0],
            ]
        elif icombo == 2:
            maxmvr, maxpackages = 2, 3
            mvrpack = [["SFR-2"], ["SFR-3"], ["SFR-4"]]
            mvrperioddata = [
                ["SFR-2", 11, "SFR-4", 0, "FACTOR", 1.0],
                ["SFR-3", 12, "SFR-4", 0, "FACTOR", 1.0],
            ]
        mvr = flopy.mf6.ModflowGwfmvr(
            gwf,
            maxmvr=maxmvr,
            print_flows=True,
            maxpackages=maxpackages,
            packages=mvrpack,
            perioddata=mvrperioddata,
        )

    if transport_on:

        gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname)
        sim.register_ims_package(imsgwt, [gwt.name])

        # ims
        hclose = 0.001
        rclose = 0.001
        nouter = 50
        ninner = 20
        relax = 0.97


        dis = flopy.mf6.ModflowGwtdis(
            gwt,
            nlay=nlay,
            nrow=nrow,
            ncol=ncol,
            delr=delr,
            delc=delc,
            top=top,
            botm=botm,
            idomain=idomain,
        )
        ic = flopy.mf6.ModflowGwtic(gwt, strt=0.0)
        mst = flopy.mf6.ModflowGwtmst(gwt, porosity=0.3)
        adv = flopy.mf6.ModflowGwtadv(gwt, scheme="TVD")
        dsp = flopy.mf6.ModflowGwtdsp(gwt, alh=20.0, ath1=2, atv=0.2)
        sourcerecarray = [()]
        ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)

        cnclist = [
            [(0, 0, 11), 500.0],
            [(0, 0, 12), 500.0],
            [(0, 0, 13), 500.0],
            [(0, 0, 14), 500.0],
            [(1, 0, 11), 500.0],
            [(1, 0, 12), 500.0],
            [(1, 0, 13), 500.0],
            [(1, 0, 14), 500.0],
        ]
        if icombo == 1:
            cnc = flopy.mf6.ModflowGwtcnc(
                gwt,
                maxbound=len(cnclist),
                stress_period_data=cnclist,
                save_flows=False,
                pname="CNC-1",
            )

        if icombo == 1:
            lktpackagedata = [(0, 0.0, 99.0, 999.0, "mylake1"),]
            lktperioddata = [(0, "STATUS", "ACTIVE")]
        elif icombo == 2:
            lktpackagedata = [(0, 0.0, 99.0, 999.0, "mylake2"),]
            lktperioddata = [(0, "STATUS", "ACTIVE")]
        else:
            raise Exception("something is not right")
        lkt_obs = {
            (gwtname + ".lkt.obs.csv",): [
                ("lkt1conc", "CONCENTRATION", 1),
            ],
        }
        lkt_obs["digits"] = 7
        lkt_obs["print_input"] = True
        lkt_obs["filename"] = gwtname + ".lkt.obs"

        if lkt_on:
            lkt = flopy.mf6.modflow.ModflowGwtlkt(
                gwt,
                boundnames=True,
                save_flows=True,
                print_input=True,
                print_flows=True,
                print_concentration=True,
                concentration_filerecord=gwtname + ".lkt.bin",
                budget_filerecord=gwtname + ".lkt.bud",
                packagedata=lktpackagedata,
                lakeperioddata=lktperioddata,
                observations=lkt_obs,
                pname="LAK-1",
                auxiliary=["aux1", "aux2"],
            )

        #
        if sft_on:

            if icombo == 1:
                isfrseglist = [1]
            else:
                isfrseglist = [2, 3, 4]

            for isfrseg in isfrseglist:
                pname = f"sfr-{isfrseg}"
                print(f"getting package {pname} from {gwf.name}")
                sfrpack = gwf.get_package(pname)
                nreaches = sfrpack.nreaches.get_data()
                sftpackagedata = []
                for irno in range(nreaches):
                    t = (irno, 0.0, 99.0, 999.0, "myreach{}".format(irno + 1))
                    sftpackagedata.append(t)

                sft_obs = {
                    (gwtname + f".sft{isfrseg}.obs.csv",): [
                        (f"sft{i + 1}conc", "CONCENTRATION", i + 1)
                        for i in range(nreaches)
                    ]
                }
                # append additional obs attributes to obs dictionary
                sft_obs["digits"] = 7
                sft_obs["print_input"] = True
                sft_obs["filename"] = gwtname + f".sft{isfrseg}.obs"

                sft = flopy.mf6.modflow.ModflowGwtsft(
                    gwt,
                    boundnames=True,
                    save_flows=True,
                    print_input=True,
                    print_flows=True,
                    print_concentration=True,
                    concentration_filerecord=gwtname + f".sft{isfrseg}.bin",
                    budget_filerecord=gwtname + f".sft{isfrseg}.bud",
                    packagedata=sftpackagedata,
                    reachperioddata=None,
                    observations=sft_obs,
                    flow_package_name=f"SFR-{isfrseg}",
                    pname=f"SFT-{isfrseg}",
                    filename=f"{gwtname}.sft{isfrseg}",
                    auxiliary=["aux1", "aux2"],
                )

        # mover transport package
        if within_model_mvt_on:
            mvt = flopy.mf6.modflow.ModflowGwtmvt(gwt)

        oc = flopy.mf6.ModflowGwtoc(
            gwt,
            budget_filerecord="{}.cbc".format(gwtname),
            concentration_filerecord="{}.ucn".format(gwtname),
            concentrationprintrecord=[
                ("COLUMNS", ncol, "WIDTH", 15, "DIGITS", 6, "GENERAL")
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
            filename=f"{gwfname}-{gwtname}.exg",
        )

    return sim


def make_concentration_vs_time(sim, ws):
    print("making concentration versus time plot...")

    times = None
    lkaconc = None
    if lkt_on:
        # northern model
        gwt = sim.get_model(gwtnames[0])
        bobj = gwt.lkt.output.concentration()
        lkaconc = bobj.get_alldata()[:, 0, 0, :]
        times = bobj.times
        bobj.file.close()

    sft3outflowconc = None
    sft4outflowconc = None
    if sft_on:

        # get southern model
        gwt = sim.get_model(gwtnames[1])
        sftpack = gwt.get_package(f"sft-3")
        times = sftpack.output.concentration().times
        conc = sftpack.output.concentration().get_alldata()[:, 0, 0, :]
        sft3outflowconc = conc[:, -1] # last reach
        sftpack = gwt.get_package(f"sft-4")
        conc = sftpack.output.concentration().get_alldata()[:, 0, 0, :]
        sft4outflowconc = conc[:, -1] # last reach

    if times is not None:
        import matplotlib.pyplot as plt

        plt.figure(figsize=(8, 5))
        times = np.array(times) / 365.0
        if lkaconc is not None:
            plt.plot(times, lkaconc[:, 0], "b-", label="Lake 1")
        if sft3outflowconc is not None:
            plt.plot(times, sft3outflowconc, "r-", label="Stream segment 3")
        if sft4outflowconc is not None:
            plt.plot(times, sft4outflowconc, "g-", label="Stream segment 4")
        plt.legend()
        plt.ylim(0, 50)
        plt.xlim(0, 25)
        plt.xlabel("TIME, IN YEARS")
        plt.ylabel("SIMULATED BORON CONCENTRATION,\nIN MICROGRAMS PER LITER")
        plt.draw()
        fname = os.path.join(ws, "fig-concentration_vs_time.png")
        print(f"Creating {fname}")
        plt.savefig(fname)

    return


def make_head_map(sim, ws):
    print("making head map...")

    import matplotlib.pyplot as plt
    levels = np.arange(20, 60, 1)
    fig, axs = plt.subplots(
        1, 2, figsize=(8, 5), dpi=300, tight_layout=True
    )

    # push combo heads into global head
    head_global = np.ones(idomain.shape, dtype=float) * 1.e30
    print("head global shape", head_global.shape)
    for icombo in [1, 2]:
        gwfname = gwfnames[icombo - 1]
        gwtname = gwtnames[icombo - 1]
        gwf = sim.get_model(gwfname)
        gwt = sim.get_model(gwtname)
        head = gwf.output.head().get_data()
        print("head shape", head.shape)
        head_idx = np.where(head != 1.e30)
        head_global[head_idx] = head[head_idx]

        if lake_on:
            stage = gwf.lak.output.stage().get_data().flatten()
            il, jl = np.where(lakibd == icombo)
            for i, j in zip(il, jl):
                ilak = 0
                lake_stage = stage[ilak]
                head_global[0, i, j] = lake_stage

    # plot layer 1 and layer 2
    for ilay in [0, 1]:
        ax = axs[ilay]
        pmv = flopy.plot.PlotMapView(model=gwf, ax=ax, layer=ilay)
        #pmv.plot_grid()
        pmv.plot_array(lakibd, masked_values=[0], alpha=0.2)
        pmv.plot_ibound(idomain)
        pmv.plot_bc(name="CHD-1", color="blue")
        cs = pmv.contour_array(head_global, levels=levels, masked_values=[1.0e30])
        ax.clabel(cs, cs.levels[::5], fmt="%1.0f", colors="b")
        ax.set_title(f"Model layer {ilay + 1}")

    fname = os.path.join(ws, "fig-head.png")
    print(f"Creating {fname}")
    plt.savefig(fname)

    return


def make_concentration_map(sim, ws):
    print("making concentration map...")

    import matplotlib.pyplot as plt
    levels = [
        1,
        10,
        25,
        50,
        100,
        150,
        200,
        250,
        300,
        350,
        400,
        450,
        500,
    ]
    fig, axs = plt.subplots(
        2, 2, figsize=(5, 7), dpi=300, tight_layout=True
    )

    # push combo concentrations into global concentration
    concentration_global = np.ones(idomain.shape, dtype=float) * 1.e30
    print("concentration global shape", concentration_global.shape)
    for icombo in [1, 2]:
        gwfname = gwfnames[icombo - 1]
        gwtname = gwtnames[icombo - 1]
        gwf = sim.get_model(gwfname)
        gwt = sim.get_model(gwtname)
        conc = gwt.output.concentration().get_data()
        print("conc shape", conc.shape)
        conc_idx = np.where(conc != 1.e30)
        concentration_global[conc_idx] = conc[conc_idx]

        if lake_on and lkt_on:
            lake_concs = gwt.lkt.output.concentration().get_data().flatten()
            il, jl = np.where(lakibd == icombo)
            for i, j in zip(il, jl):
                ilak = 0
                lake_conc = lake_concs[ilak]
                concentration_global[0, i, j] = lake_conc

    # plot layer 1 and layer 2
    for iplot, ilay in enumerate([0, 2, 4, 7]):
        ax = axs.flatten()[iplot]
        pmv = flopy.plot.PlotMapView(model=gwt, ax=ax, layer=ilay)
        #pmv.plot_grid()
        pmv.plot_array(lakibd, masked_values=[0], alpha=0.2)
        pmv.plot_ibound(idomain)
        #pmv.plot_bc(name="CHD-1", color="blue")
        cs = pmv.contour_array(concentration_global, levels=levels, masked_values=[1.0e30])
        ax.clabel(cs, cs.levels[::1], fmt="%1.0f", colors="b")
        ax.set_title(f"Model layer {ilay + 1}")

    fname = os.path.join(ws, "fig-concentration.png")
    print(f"Creating {fname}")
    plt.savefig(fname)

    return


def eval_results(sim):
    print("evaluating results...")

    makeplot = False
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "--makeplot":
            makeplot = True

    ws = exdirs[sim.idxsim]
    simfp = flopy.mf6.MFSimulation.load(sim_ws=ws)

    if makeplot:
        make_head_map(simfp, ws)
        if transport_on:
            make_concentration_vs_time(simfp, ws)
            make_concentration_map(simfp, ws)

    # ensure concentrations were saved
    ws = exdirs[sim.idxsim]
    gwfname = gwfnames[0]
    gwtname = gwtnames[0]

    lkaconc = None
    if lkt_on and transport_on:
        fname = gwtname + ".lkt.bin"
        fname = os.path.join(ws, fname)
        bobj = flopy.utils.HeadFile(
            fname, precision="double", text="concentration"
        )
        lkaconc = bobj.get_alldata()[:, 0, 0, :]
        times = bobj.times
        bobj.file.close()

    sfaconc = None
    if sft_on and transport_on:
        fname = gwtname + ".sft.bin"
        fname = os.path.join(ws, fname)
        bobj = flopy.utils.HeadFile(
            fname, precision="double", text="concentration"
        )
        sfaconc = bobj.get_alldata()[:, 0, 0, :]
        times = bobj.times
        bobj.file.close()

    # set atol
    atol = 0.02

    # check simulated concentration in lak 1 and 2 sfr reaches
    if lkaconc is not None:
        res_lak1 = lkaconc[:, 0]
        ans_lak1 = [
            -1.73249951e-19,
            5.97398983e-02,
            4.18358112e-01,
            1.48857598e00,
            3.63202585e00,
            6.92925430e00,
            1.11162776e01,
            1.57328143e01,
            2.03088745e01,
            2.45013060e01,
            2.81200704e01,
            3.11132152e01,
            3.34833369e01,
            3.53028319e01,
            3.66693021e01,
            3.76781530e01,
            3.84188513e01,
            3.89615387e01,
            3.93577458e01,
            3.96464993e01,
            3.98598113e01,
            4.00184878e01,
            4.01377654e01,
            4.02288674e01,
            4.02998291e01,
            4.03563314e01,
        ]
        ans_lak1 = np.array(ans_lak1)
        d = res_lak1 - ans_lak1
        msg = "{} {} {}".format(res_lak1, ans_lak1, d)
        assert np.allclose(res_lak1, ans_lak1, atol=atol), msg

    if sfaconc is not None:
        res_sfr3 = sfaconc[:, 30]
        ans_sfr3 = [
            -7.67944651e-23,
            5.11358249e-03,
            3.76169957e-02,
            1.42055634e-01,
            3.72438193e-01,
            7.74112522e-01,
            1.37336373e00,
            2.18151231e00,
            3.19993561e00,
            4.42853144e00,
            5.85660993e00,
            7.46619448e00,
            9.22646330e00,
            1.11069607e01,
            1.30764504e01,
            1.50977917e01,
            1.71329980e01,
            1.91636634e01,
            2.11530199e01,
            2.30688490e01,
            2.48821059e01,
            2.65691424e01,
            2.81080543e01,
            2.94838325e01,
            3.06909748e01,
            3.17352915e01,
        ]
        ans_sfr3 = np.array(ans_sfr3)
        d = res_sfr3 - ans_sfr3
        msg = "{} {} {}".format(res_sfr3, ans_sfr3, d)
        assert np.allclose(res_sfr3, ans_sfr3, atol=atol), msg

        res_sfr4 = sfaconc[:, 37]
        ans_sfr4 = [
            -2.00171747e-20,
            3.55076535e-02,
            2.49465789e-01,
            8.91299656e-01,
            2.18622372e00,
            4.19920114e00,
            6.79501651e00,
            9.72255743e00,
            1.27208739e01,
            1.55989390e01,
            1.82462345e01,
            2.06258607e01,
            2.27255881e01,
            2.45721928e01,
            2.62061367e01,
            2.76640442e01,
            2.89788596e01,
            3.01814571e01,
            3.12842113e01,
            3.22945541e01,
            3.32174210e01,
            3.40539043e01,
            3.48027700e01,
            3.54636082e01,
            3.60384505e01,
            3.65330352e01,
        ]
        ans_sfr4 = np.array(ans_sfr4)
        d = res_sfr4 - ans_sfr4
        msg = "{} {} {}".format(res_sfr4, ans_sfr4, d)
        assert np.allclose(res_sfr4, ans_sfr4, atol=atol), msg

    # uncomment when testing
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
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
