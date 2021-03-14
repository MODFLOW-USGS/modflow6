import os

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

from framework import testing_framework, running_on_CI
from simulation import Simulation

ex = [
    "csub_sub02a",
    "csub_sub02b",
    "csub_sub02c",
    "csub_sub02d",
    "csub_sub02e",
]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
cg_ske = 1.14e-3 / (500.0 - 20.0)
cg_S = cg_ske * (500.0 - 20.0)
ss = [cg_S, cg_S, cg_ske, cg_ske, cg_S]
storagecoeff = [True, True, False, False, True]
cdelay = [False, True, False, True, True]
ndelaycells = [None, 19, None, 19, 19]

# run all examples on Travis
# continuous_integration = [True for idx in range(len(exdirs))]
# the delay bed problems only run on the development version of MODFLOW-2005
# set travis to True when version 1.13.0 is released
continuous_integration = [True, False, True, False, False]

# set replace_exe to None to use default executable
replace_exe = {"mf2005": "mf2005devdbl"}

# static model data
nlay, nrow, ncol = 1, 1, 1
nper = 10
perlen = [182.625 for i in range(nper)]
nstp = [10 for i in range(nper)]
tsmult = [1.05 for i in range(nper)]
steady = [False for i in range(nper)]
delr, delc = 1000.0, 1000.0
top = -100.0
botm = [-600.0]
strt = 0.0
hnoflo = 1e30
hdry = -1e30
hk = 1e6
laytyp = [0]
sy = 0.0

nouter, ninner = 1000, 300
hclose, rclose, relax = 1e-6, 1e-6, 0.97

tdis_rc = []
for idx in range(nper):
    tdis_rc.append((perlen[idx], nstp[idx], tsmult[idx]))

ib = 1

wd = {}
wd6 = {}
for i in range(nper):
    if i % 2 == 0:
        q = -118.3
    else:
        q = 23.66
    d = [[0, 0, 0, q]]
    d6 = [[(0, 0, 0), q]]
    wd[i] = d
    wd6[i] = d6

# sub data
cc = 0.005
cr = 5e-5
void = 0.82
theta = void / (1.0 + void)
kv = 9.72e-6
sgm = 0.0
sgs = 0.0
ini_stress = 0.0
thick = [20.0]
sfe = cr * thick[0]
sfv = cc * thick[0]
lnd = [0]
ldnd = [0]
dp = [[kv, cr, cc]]

ds15 = [0, 2052, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
ds16 = [0, 9, 0, 9, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]


def get_model(idx, dir):
    name = ex[idx]

    ss = 1.14e-3
    sc6 = True
    if not storagecoeff[idx]:
        ss /= top - botm[0]
        sc6 = None

    if cdelay[idx]:
        nndb = 0
        ndb = 1
        cdelays = "delay"
    else:
        nndb = 1
        ndb = 0
        cdelays = "nodelay"

    sub6 = [
        [
            0,
            (0, 0, 0),
            cdelays,
            ini_stress,
            thick[0],
            1.0,
            cc,
            cr,
            theta,
            kv,
            0.0,
            "db01",
        ]
    ]

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
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=name, model_nam_file="{}.nam".format(name)
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
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
    )
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        filename="{}.dis".format(name),
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename="{}.ic".format(name))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=False, icelltype=laytyp, k=hk, k33=hk
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=0.0,
        sy=sy,
        storagecoefficient=sc6,
        transient={0: True},
    )

    # wel files
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        print_input=True,
        print_flows=True,
        maxbound=1,
        stress_period_data=wd6,
        save_flows=False,
    )

    # csub files
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        print_input=True,
        boundnames=True,
        head_based=True,
        ndelaycells=ndelaycells[idx],
        ninterbeds=1,
        beta=0.0,
        cg_ske_cr=cg_ske,
        packagedata=sub6,
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord="{}.cbc".format(name),
        head_filerecord="{}.hds".format(name),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # build MODFLOW-2005 files
    ws = os.path.join(dir, "mf2005")
    mc = flopy.modflow.Modflow(name, model_ws=ws)
    dis = flopy.modflow.ModflowDis(
        mc,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        nper=nper,
        perlen=perlen,
        nstp=nstp,
        tsmult=tsmult,
        steady=steady,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )
    bas = flopy.modflow.ModflowBas(
        mc, ibound=ib, strt=strt, hnoflo=hnoflo, stoper=0.01
    )
    lpf = flopy.modflow.ModflowLpf(
        mc,
        laytyp=laytyp,
        hk=hk,
        vka=hk,
        ss=ss,
        sy=sy,
        constantcv=True,
        storagecoefficient=storagecoeff[idx],
        hdry=hdry,
    )
    wel = flopy.modflow.ModflowWel(mc, stress_period_data=wd)
    sub = flopy.modflow.ModflowSub(
        mc,
        ndb=ndb,
        nndb=nndb,
        nn=10,
        idbit=1,
        isuboc=1,
        ln=lnd,
        ldn=ldnd,
        rnb=[1.0],
        dp=dp,
        dz=thick,
        dhc=ini_stress,
        dstart=ini_stress,
        hc=ini_stress,
        sfe=sfe,
        sfv=sfv,
        ids15=ds15,
        ids16=ds16,
    )
    oc = flopy.modflow.ModflowOc(mc, stress_period_data=None)
    pcg = flopy.modflow.ModflowPcg(
        mc,
        mxiter=nouter,
        iter1=ninner,
        hclose=hclose,
        rclose=rclose,
        relax=relax,
        ihcofadd=1,
    )

    return sim, mc


# - No need to change any code below
def build_models():
    for idx, dir in enumerate(exdirs):
        sim, mc = get_model(idx, dir)
        sim.write_simulation()
        if mc is not None:
            mc.write_input()
    return


def test_mf6model():
    # determine if running on Travis or GitHub actions
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        if is_CI and not continuous_integration[idx]:
            continue
        yield test.run_mf6, Simulation(dir, exe_dict=r_exe)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for dir in exdirs:
        sim = Simulation(dir, exe_dict=replace_exe)
        test.run_mf6(sim)

    return


# use python test_gwf_csub_sub02.py --mf2005 mf2005devdbl
if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
