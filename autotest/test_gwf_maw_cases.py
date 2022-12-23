import os
from collections import namedtuple
from types import SimpleNamespace

import flopy
import numpy as np
from modflow_devtools.case import Case
from pytest_cases import parametrize

well1 = SimpleNamespace(
    observations={"maw_obs.csv": [("mh1", "head", 1)]},
    packagedata=[[0, 0.1, 50.0, 100.0, "THIEM", 1]],
    connectiondata=[[0, 0, (0, 0, 1), 100.0, 50.0, 1.0, 0.1]],
    perioddata=[[0, "rate", 0.0]],
)

well2 = SimpleNamespace(
    observations={"maw_obs.csv": [("mh1", "head", 1)]},
    packagedata=[
        [0, 0.1, 0.0, 100.0, "THIEM", 1],
        [1, 0.1, 0.0, 100.0, "THIEM", 1],
    ],
    connectiondata=[
        [0, 0, (0, 0, 1), 100.0, 0.0, 1.0, 0.1],
        [1, 0, (0, 0, 1), 100.0, 0.0, 1.0, 0.1],
    ],
    perioddata={
        0: [
            [0, "rate", -20.0],
            [0, "status", "inactive"],
            [0, "rate_scaling", 1.0, 15.0],
            [1, "rate", -30.0],
            [1, "status", "inactive"],
            [1, "rate_scaling", 5.0, 15.0],
        ],
        1: [
            [0, "rate", -110.0],
            [0, "status", "active"],
            [1, "rate", -130.0],
            [1, "status", "active"],
        ],
        3: [[0, "status", "inactive"]],
        4: [[0, "status", "active"]],
    },
)


def well3(name):
    perioddata = {
        "maw03a": [
            (0, "rate", 2000.0),
        ],
        "maw03b": [(0, "rate", 2000.0), (0, "head_limit", 0.4)],
        "maw03c": [(0, "rate", 2000.0), (0, "rate_scaling", 0.0, 1.0)],
    }
    wellbottom = -1000
    return SimpleNamespace(
        observations={
            f"{name}.maw.obs.csv": [
                ("m1head", "head", (0,)),
                ("m1rate", "rate", (0,)),
            ]  # is this index one-based? Not if in a tuple
        },
        packagedata=[[0, 0.15, wellbottom, 0.0, "THIEM", 1]],
        connectiondata=[[0, 0, (0, 50, 50), 0.0, wellbottom, 0.0, 0.0]],
        perioddata=perioddata[name],
    )


def well4(case, condeqn):
    radius0 = np.sqrt(
        case.delr[case.nhalf] * case.delr[case.nhalf] / (8.0 * np.pi)
    )
    radius = 0.25
    sradius0 = radius + 0.1
    sradius = [sradius0, sradius0, sradius0, sradius0, sradius0, radius0 * 1.5]
    packagedata = [[0, case.radius, case.botm[-1], case.strt, condeqn, 2]]
    connectiondata = [
        [
            0,
            0,
            (0, case.nhalf, case.nhalf),
            case.top,
            case.botm[0],
            case.hks,
            sradius[case.name],
        ],
        [
            0,
            1,
            (1, case.nhalf, case.nhalf),
            case.botm[0],
            case.botm[1],
            case.hks,
            sradius[case.name],
        ],
    ]
    perioddata = {1: [[0, "RATE", case.wellq]]}
    return SimpleNamespace(
        print_input=True,
        no_well_storage=True,
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=perioddata,
    )


class GwfMawCases:
    """
    Test cases for multi-aquifer well groundwater flow models.
    """

    budtol = 1e-2
    bud_lst = ["GWF_IN", "GWF_OUT", "RATE_IN", "RATE_OUT"]

    case1 = Case(
        name="maw01",
        nlay=1,
        nrow=1,
        ncol=3,
        nper=3,
        delr=300,
        delc=300,
        krylov="CG",
        perlen=3 * [1],
        nstp=3 * [1],
        tsmult=3 * [1],
        well=well1,
        strt=100,
        hk=1,
        nouter=100,
        ninner=300,
        hclose=1e-9,
        rclose=1e-3,
        relaxation_factor=1,
        newton=None,
        compare=False,
    )
    cases1 = [
        case1,
        case1.copy_update(name="maw01nwt", krylov="BICGSTAB", newton="NEWTON"),
        case1.copy_update(
            name="maw01nwtur",
            krylov="BICGSTAB",
            newton="NEWTON UNDER_RELAXATION",
        ),
    ]

    @parametrize(data=cases1, ids=[c.name for c in cases1])
    def case_1(self, tmpdir, targets, data):
        sim = flopy.mf6.MFSimulation(
            sim_name=data.name,
            version="mf6",
            exe_name=targets["mf6"],
            sim_ws=str(tmpdir),
        )

        # create tdis package
        tdis_rc = [
            (data.perlen[i], data.nstp[i], data.tsmult[i])
            for i in range(data.nper)
        ]
        tdis = flopy.mf6.ModflowTdis(
            sim, time_units="DAYS", nper=data.nper, perioddata=tdis_rc
        )

        # create gwf model
        gwf = flopy.mf6.MFModel(
            sim,
            model_type="gwf6",
            modelname=data.name,
            model_nam_file=f"{data.name}.nam",
        )
        gwf.name_file.newtonoptions = data.newton

        # create iterative model solution and register the gwf model with it
        ims = flopy.mf6.ModflowIms(
            sim,
            print_option="SUMMARY",
            outer_dvclose=data.hclose,
            outer_maximum=data.nouter,
            under_relaxation="NONE",
            inner_maximum=data.ninner,
            inner_dvclose=data.hclose,
            rcloserecord=data.rclose,
            linear_acceleration=data.krylov,
            scaling_method="NONE",
            reordering_method="NONE",
            relaxation_factor=data.relaxation_factor,
        )
        sim.register_ims_package(ims, [gwf.name])

        dis = flopy.mf6.ModflowGwfdis(
            gwf,
            nlay=data.nlay,
            nrow=data.nrow,
            ncol=data.ncol,
            delr=data.delr,
            delc=data.delc,
            top=100.0,
            botm=0.0,
            idomain=1,
            filename=f"{data.name}.dis",
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(
            gwf, strt=data.strt, filename=f"{data.name}.ic"
        )

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf,
            save_flows=True,
            icelltype=1,
            k=data.hk,
            k33=data.hk,
            filename=f"{data.name}.npf",
        )
        # storage
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=True,
            iconvert=1,
            ss=0.0,
            sy=0.1,
            steady_state={0: True},
            # transient={1: False},
            filename=f"{data.name}.sto",
        )

        # chd files
        chdlist0 = []
        chdlist0.append([(0, 0, 0), 100.0])
        chdlist0.append([(0, 0, 2), 100.0])

        chdlist1 = []
        chdlist1.append([(0, 0, 0), 25.0])
        chdlist1.append([(0, 0, 2), 25.0])

        chdspdict = {0: chdlist0, 1: chdlist1, 2: chdlist0}
        chd = flopy.mf6.ModflowGwfchd(
            gwf,
            stress_period_data=chdspdict,
            save_flows=False,
            filename=f"{data.name}.chd",
        )

        # wel files
        # wel = flopy.mf6.ModflowGwfwel(gwf, print_input=True, print_flows=True,
        #                              maxbound=len(ws),
        #                              periodrecarray=wd6,
        #                              save_flows=False)
        # MAW
        maw = flopy.mf6.ModflowGwfmaw(
            gwf,
            filename=f"{data.name}.maw",
            print_input=True,
            print_head=True,
            print_flows=True,
            save_flows=True,
            observations=data.well.observations,
            packagedata=data.well.packagedata,
            connectiondata=data.well.connectiondata,
            perioddata=data.well.perioddata,
        )

        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord=f"{data.name}.cbc",
            head_filerecord=f"{data.name}.hds",
            headprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("HEAD", "ALL")],
            printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
            filename=f"{data.name}.oc",
        )

        return data, sim, None, self.eval_1

    def eval_1(self, config, data):
        print("evaluating MAW heads...")

        # MODFLOW 6 maw results
        fpth = os.path.join(config.simpath, "maw_obs.csv")
        tc = np.genfromtxt(fpth, names=True, delimiter=",")

        # create known results array
        tc0 = np.array([100.0, 25.0, 100.0])

        # calculate maximum absolute error
        diff = tc["MH1"] - tc0
        diffmax = np.abs(diff).max()
        dtol = 1e-9
        msg = f"maximum absolute maw head difference ({diffmax}) "

        if diffmax > dtol:
            config.success = False
            msg += f"exceeds {dtol}"
            assert diffmax < dtol, msg
        else:
            config.success = True
            print("    " + msg)

    case2 = Case(
        name="maw02",
        krylov="CG",
        nlay=1,
        nrow=1,
        ncol=3,
        nper=5,
        delr=300,
        delc=300,
        perlen=5 * [1],
        nstp=5 * [1],
        tsmult=5 * [1],
        well=well2,
        strt=100,
        hk=1,
        nouter=100,
        ninner=300,
        hclose=1e-9,
        rclose=1e-3,
        relaxation_factor=1,
        compare=False,
    )
    cases2 = [case2]

    @parametrize(data=cases2, ids=[c.name for c in cases2])
    def case_2(self, tmpdir, targets, data):
        name = data.name
        ws = str(tmpdir)
        sim = flopy.mf6.MFSimulation(
            sim_name=name, version="mf6", exe_name=targets["mf6"], sim_ws=ws
        )

        # create tdis package
        tdis_rc = [
            (data.perlen[i], data.nstp[i], data.tsmult[i])
            for i in range(data.nper)
        ]
        tdis = flopy.mf6.ModflowTdis(
            sim, time_units="DAYS", nper=data.nper, perioddata=tdis_rc
        )

        # create gwf model
        gwf = flopy.mf6.MFModel(
            sim,
            model_type="gwf6",
            modelname=name,
            model_nam_file=f"{name}.nam",
        )

        # create iterative model solution and register the gwf model with it
        ims = flopy.mf6.ModflowIms(
            sim,
            print_option="SUMMARY",
            outer_dvclose=data.hclose,
            outer_maximum=data.nouter,
            under_relaxation="NONE",
            inner_maximum=data.ninner,
            inner_dvclose=data.hclose,
            rcloserecord=data.rclose,
            linear_acceleration=data.krylov,
            scaling_method="NONE",
            reordering_method="NONE",
            relaxation_factor=data.relaxation_factor,
        )
        sim.register_ims_package(ims, [gwf.name])

        dis = flopy.mf6.ModflowGwfdis(
            gwf,
            nlay=data.nlay,
            nrow=data.nrow,
            ncol=data.ncol,
            delr=data.delr,
            delc=data.delc,
            top=100.0,
            botm=0.0,
            idomain=1,
            filename=f"{name}.dis",
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=data.strt, filename=f"{name}.ic")

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf,
            save_flows=True,
            icelltype=1,
            k=data.hk,
            k33=data.hk,
            filename=f"{name}.npf",
        )
        # storage
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=True,
            iconvert=1,
            ss=0.0,
            sy=0.1,
            steady_state={0: True},
            # transient={1: False},
            filename=f"{name}.sto",
        )

        # chd files
        chdlist0 = []
        chdlist0.append([(0, 0, 0), 100.0])
        chdlist0.append([(0, 0, 2), 100.0])

        chdlist1 = []
        chdlist1.append([(0, 0, 0), 25.0])
        chdlist1.append([(0, 0, 2), 25.0])

        chdspdict = {0: chdlist0, 1: chdlist1, 2: chdlist0}
        chd = flopy.mf6.ModflowGwfchd(
            gwf,
            stress_period_data=chdspdict,
            save_flows=False,
            filename=f"{name}.chd",
        )

        # MAW
        maw = flopy.mf6.ModflowGwfmaw(
            gwf,
            filename=f"{name}.maw",
            budget_filerecord=f"{name}.maw.cbc",
            print_input=True,
            print_head=True,
            print_flows=True,
            save_flows=True,
            observations=data.well.observations,
            packagedata=data.well.packagedata,
            connectiondata=data.well.connectiondata,
            perioddata=data.well.perioddata,
            pname="MAW-1",
        )

        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord=f"{name}.cbc",
            head_filerecord=f"{name}.hds",
            headprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
            printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
            filename=f"{name}.oc",
        )

        return data, sim, None, self.eval_2

    def eval_2(self, config, data):
        print("evaluating MAW budgets...")

        shape3d = (data.nlay, data.nrow, data.ncol)
        size3d = data.nlay * data.nrow * data.ncol

        # get results from listing file
        fpth = os.path.join(
            config.simpath, f"{os.path.basename(config.name)}.lst"
        )
        budl = flopy.utils.Mf6ListBudget(
            fpth, budgetkey="MAW-1 BUDGET FOR ENTIRE MODEL AT END OF TIME STEP"
        )
        names = list(self.bud_lst)
        d0 = budl.get_budget(names=names)[0]
        dtype = d0.dtype
        nbud = d0.shape[0]

        # get results from cbc file
        cbc_bud = ["GWF", "RATE"]
        d = np.recarray(nbud, dtype=dtype)
        for key in self.bud_lst:
            d[key] = 0.0
        fpth = os.path.join(
            config.simpath, f"{os.path.basename(config.name)}.maw.cbc"
        )
        cobj = flopy.utils.CellBudgetFile(fpth, precision="double")
        kk = cobj.get_kstpkper()
        times = cobj.get_times()
        cbc_vals = []
        for idx, (k, t) in enumerate(zip(kk, times)):
            for text in cbc_bud:
                qin = 0.0
                qout = 0.0
                v = cobj.get_data(kstpkper=k, text=text)[0]
                if isinstance(v, np.recarray):
                    vt = np.zeros(size3d, dtype=float)
                    wq = []
                    for jdx, node in enumerate(v["node"]):
                        vt[node - 1] += v["q"][jdx]
                        wq.append(v["q"][jdx])
                    v = vt.reshape(shape3d)
                    if text == cbc_bud[-1]:
                        cbc_vals.append(wq)
                for kk in range(v.shape[0]):
                    for ii in range(v.shape[1]):
                        for jj in range(v.shape[2]):
                            vv = v[kk, ii, jj]
                            if vv < 0.0:
                                qout -= vv
                            else:
                                qin += vv
                d["totim"][idx] = t
                d["time_step"][idx] = k[0]
                d["stress_period"] = k[1]
                key = f"{text}_IN"
                d[key][idx] = qin
                key = f"{text}_OUT"
                d[key][idx] = qout

        maw_vals = [
            [0.000, 0.000],
            [-106.11303563809453, -96.22598985147631],
            [-110.000, -130.000],
            [0.0, -130.000],
            [-110.000, -130.000],
        ]

        # evaluate if well rates in cbc file are equal to expected values
        diffv = []
        for ovs, svs in zip(maw_vals, cbc_vals):
            for ov, sv in zip(ovs, svs):
                diffv.append(ov - sv)
        diffv = np.abs(np.array(diffv)).max()
        msg = f"\nmaximum absolute maw rate difference     ({diffv})\n"

        # calculate difference between water budget items in the lst and cbc files
        diff = np.zeros((nbud, len(self.bud_lst)), dtype=float)
        for idx, key in enumerate(self.bud_lst):
            diff[:, idx] = d0[key] - d[key]
        diffmax = np.abs(diff).max()
        msg += f"maximum absolute total-budget difference ({diffmax}) "

        # write summary
        fpth = os.path.join(
            config.simpath, f"{os.path.basename(config.name)}.bud.cmp.out"
        )
        f = open(fpth, "w")
        for i in range(diff.shape[0]):
            if i == 0:
                line = f"{'TIME':>10s}"
                for idx, key in enumerate(self.bud_lst):
                    line += f"{key + '_LST':>25s}"
                    line += f"{key + '_CBC':>25s}"
                    line += f"{key + '_DIF':>25s}"
                f.write(line + "\n")
            line = f"{d['totim'][i]:10g}"
            for idx, key in enumerate(self.bud_lst):
                line += f"{d0[key][i]:25g}"
                line += f"{d[key][i]:25g}"
                line += f"{diff[i, idx]:25g}"
            f.write(line + "\n")
        f.close()

        if diffmax > self.budtol or diffv > self.budtol:
            config.success = False
            msg += f"\n...exceeds {self.budtol}"
            assert diffmax < self.budtol, msg
        else:
            config.success = True
            print("    " + msg)

    case3 = Case(
        name="maw03",
        krylov="CG",
        nlay=1,
        nrow=101,
        ncol=101,
        nper=1,
        delr=142,
        delc=142,
        perlen=[1000],
        nstp=[50],
        tsmult=[1.2],
        strt=0,
        hk=10,
        nouter=100,
        ninner=100,
        hclose=1e-6,
        rclose=1e-6,
        relaxation_factor=1,
        compare=False,
    )
    cases3 = [
        case3.copy_update(
            name="maw03a",
            well=well3("maw03a"),
        ),
        case3.copy_update(name="maw03b", well=well3("maw03b")),
        case3.copy_update(name="maw03c", well=well3("maw03c")),
    ]

    @parametrize(data=cases3, ids=[c.name for c in cases3])
    def case_3(self, tmpdir, targets, data):
        top = 0.0
        botm = [-1000.0]

        tdis_rc = []
        for i in range(data.nper):
            tdis_rc.append((data.perlen[i], data.nstp[i], data.tsmult[i]))

        name = data.name
        ws = str(tmpdir)
        sim = flopy.mf6.MFSimulation(
            sim_name=name, sim_ws=ws, exe_name=targets["mf6"]
        )

        # create tdis package
        tdis = flopy.mf6.ModflowTdis(
            sim, time_units="DAYS", nper=data.nper, perioddata=tdis_rc
        )

        # create gwf model
        gwf = flopy.mf6.MFModel(
            sim,
            model_type="gwf6",
            modelname=name,
            model_nam_file=f"{name}.nam",
        )

        # create iterative model solution and register the gwf model with it
        ims = flopy.mf6.ModflowIms(
            sim,
            print_option="SUMMARY",
            outer_dvclose=data.hclose,
            outer_maximum=data.nouter,
            under_relaxation="NONE",
            inner_maximum=data.ninner,
            inner_dvclose=data.hclose,
            rcloserecord=data.rclose,
            linear_acceleration=data.krylov,
            scaling_method="NONE",
            reordering_method="NONE",
            relaxation_factor=data.relaxation_factor,
        )
        sim.register_ims_package(ims, [gwf.name])

        dis = flopy.mf6.ModflowGwfdis(
            gwf,
            nlay=data.nlay,
            nrow=data.nrow,
            ncol=data.ncol,
            delr=data.delr,
            delc=data.delc,
            top=top,
            botm=botm,
            idomain=1,
            filename=f"{name}.dis",
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(gwf, strt=data.strt, filename=f"{name}.ic")

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf,
            save_flows=True,
            icelltype=1,
            k=data.hk,
            k33=data.hk,
            filename=f"{name}.npf",
        )

        # storage
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=True,
            iconvert=0,
            ss=1.0e-5,
            sy=0.1,
            steady_state={0: False},
            transient={0: True},
            filename=f"{name}.sto",
        )

        # MAW
        maw = flopy.mf6.ModflowGwfmaw(
            gwf,
            filename=f"{name}.maw",
            print_input=True,
            print_head=True,
            print_flows=True,
            save_flows=True,
            observations=data.well.observations,
            packagedata=data.well.packagedata,
            connectiondata=data.well.connectiondata,
            perioddata=data.well.perioddata,
        )

        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord=f"{name}.cbc",
            head_filerecord=f"{name}.hds",
            headprintrecord=[
                ("COLUMNS", data.ncol, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("HEAD", "ALL")],
            printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
            filename=f"{name}.oc",
        )

        # head observations
        obs_data0 = [("head_well_cell", "HEAD", (0, 0, 0))]
        obs_recarray = {f"{name}.obs.csv": obs_data0}
        obs = flopy.mf6.ModflowUtlobs(
            gwf,
            pname="head_obs",
            filename=f"{name}.obs",
            digits=15,
            print_input=True,
            continuous=obs_recarray,
        )

        return data, sim, None, self.eval_3

    def eval_3(self, config, data):
        print("evaluating MAW heads...")

        # MODFLOW 6 maw results
        test_name = config.name
        case_name = data.name
        fpth = os.path.join(config.simpath, f"{test_name}.maw.obs.csv")
        tc = np.genfromtxt(fpth, names=True, delimiter=",")

        if case_name == "a":

            # M1RATE should be 2000.
            msg = "The injection rate should be 2000. for all times"
            assert tc["M1RATE"].min() == tc["M1RATE"].max() == 2000, msg

        elif case_name == "b":

            # M1RATE should have a minimum value less than 200 and
            # M1HEAD should not exceed 0.400001
            msg = (
                "Injection rate should fall below 200 and the head should not"
                "exceed 0.4"
            )
            assert tc["M1RATE"].min() < 200.0, msg
            assert tc["M1HEAD"].max() < 0.400001, msg

        elif case_name == "c":

            # M1RATE should have a minimum value less than 800
            # M1HEAD should not exceed 1.0.
            msg = (
                "Min injection rate should be less than 800 and well "
                "head should not exceed 1.0"
            )
            assert tc["M1RATE"].min() < 800.0 and tc["M1HEAD"].max() < 1.0, msg

    # TODO
    # case4 = Case(
    #     name='maw_iss305',
    #     krylov='CG',
    #     nlay=2,
    #     nrow=101,
    #     ncol=101,
    #     nper=2,
    #     delr=142,
    #     delc=142,
    #     perlen=[0.0, 365.0],
    #     nstp=[1, 25],
    #     tsmult=[1.0, 1.1],
    #     steady=[True, False],
    #     well=None,
    #     strt=0,
    #     hk=10,
    #     nouter=100,
    #     ninner=100,
    #     hclose=1e-9,
    #     rclose=1e-6,
    #     relax=1,
    #     require_failure=True
    # )
    # cases4 = [
    #     case4.copy_update({
    #         'name': "maw_iss305a",
    #         'well': well4(case4, "CUMULATIVE"),
    #         'require_failure': False
    #     }),
    #     case4.copy_update({
    #         'name': "maw_iss305b",
    #         'well': well4(case4, "SKIN")
    #     }),
    #     case4.copy_update({
    #         'name': "maw_iss305c",
    #         'well': well4(case4, "SKIN")
    #     }),
    #     case4.copy_update({
    #         'name': "maw_iss305d",
    #         'well': well4(case4, "SKIN")
    #     }),
    #     case4.copy_update({
    #         'name': "maw_iss305e",
    #         'well': well4(case4, "SPECIFIED")
    #     }),
    #     case4.copy_update({
    #         'name': "maw_iss305f",
    #         'well': well4(case4, "CUMULATIVE")
    #     })
    # ]

    # @parametrize(data=cases4, ids=[c['name'] for c in cases4])
    # def case_4(self, tmpdir, targets, data):
    #     pass

    # def eval_4(self, sim, data):
    #     pass
