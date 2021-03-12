import os
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

ex = ["npf02_hreweta", "npf02_hrewetb", "npf02_hrewetc", "npf02_hrewetd"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
ncols = [[15], [10, 5], [15], [10, 5]]
nlays = [1, 1, 3, 3]

# static model data
nrow = 10
nper = 2
perlen = [1.0, 1.0]
nstp = [1, 1]
tsmult = [1.0, 1.0]

lenx = 15.0 * 500.0
leny = 10.0 * 500.0
delr = lenx / float(ncols[0][0])
delc = leny / float(nrow)
top = 150.0
strt = -40.0
hk = 10.0

# rewetting
rewet_record = [("WETFCT", 1.0, "IWETIT", 1, "IHDWET", 1)]
wetdry = -0.001  # [0.001, 0.001, 0.001]

# left chd boundary for each stress period
hbndl = [100.0, 25]

nouter, ninner = 1000, 100
hclose, rclose, relax = 1e-1, 0.01, 1.0

tdis_rc = []
for i in range(nper):
    tdis_rc.append((perlen[i], nstp[i], tsmult[i]))


def get_local_data(idx):
    ncolst = ncols[idx]
    nmodels = len(ncolst)
    mnames = []
    for jdx in range(nmodels):
        mname = "gwf{}".format(jdx)
        mnames.append(mname)
    return ncolst, nmodels, mnames


def get_model(idx, dir):
    name = ex[idx]
    nlay = nlays[idx]

    if nlay == 1:
        botm = [-50.0]
    elif nlay == 3:
        botm = [50.0, 0.0, -50.0]

    c6left = []
    c6right = []
    vl = hbndl[0]
    vr = strt
    for k in range(nlay):
        for i in range(nrow):
            if botm[k] < vl:
                c6left.append([(k, i, 0), vl])
            if botm[k] < vr:
                c6right.append([(k, i, ncols[idx][-1] - 1), vr])
    cd6left = {0: c6left}
    cd6right = {0: c6right}
    c6left = []
    vl = hbndl[1]
    for k in range(nlay):
        for i in range(nrow):
            if botm[k] < vl:
                c6left.append([(k, i, 0), vl])
    cd6left[1] = c6left

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )
    # set ims csv files
    csv0 = "{}.outer.ims.csv".format(name)
    csv1 = "{}.inner.ims.csv".format(name)

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        csv_outer_output_filerecord=csv0,
        csv_inner_output_filerecord=csv1,
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

    # set local data for this model
    ncolst, nmodels, mnames = get_local_data(idx)

    sim.register_ims_package(ims, mnames)

    # create exchange file, if needed
    if nmodels > 1:
        exchd = []
        for k in range(nlay):
            for i in range(nrow):
                t = []
                t.append((k, i, ncolst[0] - 1))
                t.append((k, i, 0))
                t.append(1)
                t.append(delr / 2.0)
                t.append(delr / 2.0)
                t.append(delc)
                exchd.append(t)
        excf = flopy.mf6.ModflowGwfgwf(
            sim,
            exgtype="GWF6-GWF6",
            nexg=len(exchd),
            exgmnamea=mnames[0],
            exgmnameb=mnames[1],
            exchangedata=exchd,
        )

    # create gwf model
    for jdx in range(nmodels):
        mname = mnames[jdx]

        gwf = flopy.mf6.ModflowGwf(
            sim, modelname=mname, model_nam_file="{}.nam".format(mname)
        )

        dis = flopy.mf6.ModflowGwfdis(
            gwf,
            nlay=nlay,
            nrow=nrow,
            ncol=ncolst[jdx],
            delr=delr,
            delc=delc,
            top=top,
            botm=botm,
            filename="{}.dis".format(mname),
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwfic(
            gwf, strt=strt, filename="{}.ic".format(mname)
        )

        # node property flow
        npf = flopy.mf6.ModflowGwfnpf(
            gwf,
            save_flows=False,
            rewet_record=rewet_record,
            icelltype=1,
            k=hk,
            wetdry=wetdry,
        )

        # chd files
        if jdx == 0:
            fn = "{}.chd1.chd".format(mname)
            chd1 = flopy.mf6.modflow.ModflowGwfchd(
                gwf,
                stress_period_data=cd6left,
                save_flows=False,
                filename=fn,
                pname="chd1",
                print_input=True,
            )
        if jdx == nmodels - 1:
            fn = "{}.chd2.chd".format(mname)
            chd2 = flopy.mf6.modflow.ModflowGwfchd(
                gwf,
                stress_period_data=cd6right,
                save_flows=False,
                filename=fn,
                pname="chd2",
                print_input=True,
            )

        # output control
        oc = flopy.mf6.ModflowGwfoc(
            gwf,
            budget_filerecord="{}.cbc".format(mname),
            head_filerecord="{}.hds".format(mname),
            headprintrecord=[
                ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
            ],
            saverecord=[("HEAD", "LAST")],
            printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        )

    return sim


def build_models():
    for idx, dir in enumerate(exdirs):
        sim = get_model(idx, dir)
        sim.write_simulation()
    return


def eval_hds(sim):
    print("evaluating rewet heads...")

    hdata01lay = [
        [
            1.000000000000000000e02,
            9.491194675652451451e01,
            8.963852784804279850e01,
            8.415783779990768210e01,
            7.844327192271720151e01,
            7.246197081412121577e01,
            6.617253666303386694e01,
            5.952154998933371388e01,
            5.243800267025346074e01,
            4.482387942676999870e01,
            3.653700495397358594e01,
            2.735652176730824436e01,
            1.690257311648982963e01,
            4.399393520373488187e00,
            -4.000000000000000000e01,
        ],
        [
            2.500000000000000000e01,
            2.236586013240325954e01,
            1.963190572619273766e01,
            1.678583576594299842e01,
            1.381260947657045435e01,
            1.069347651326954285e01,
            7.404555602569254269e00,
            3.914620350596174969e00,
            1.814633055691637076e-01,
            -3.854468987316777451e00,
            -8.282210428071218544e00,
            -1.324636654341384912e01,
            -1.901457683515873498e01,
            -2.622133377686074951e01,
            -4.000000000000000000e01,
        ],
    ]
    hdata03lay = [
        [
            1.000000000000000000e02,
            9.496635413928621006e01,
            8.974621554975979620e01,
            8.432145909121111060e01,
            7.866533423602578523e01,
            7.274409709981135563e01,
            6.651345215582828985e01,
            5.991038870912337444e01,
            5.278756038520420901e01,
            4.516004691353590772e01,
            3.696996891688258557e01,
            2.789393018156467008e01,
            1.752424401606568338e01,
            4.558574625543450942e00,
            -4.000000000000000000e01,
        ],
        [
            2.500000000000000000e01,
            2.237276761469625441e01,
            1.964557053969263478e01,
            1.680659188031021856e01,
            1.384042391509487047e01,
            1.072732357470741604e01,
            7.440599114767919353e00,
            3.939438433708895015e00,
            9.093639055775014357e-02,
            -3.940378740567167970e00,
            -8.354930949636699467e00,
            -1.330380417477578270e01,
            -1.905636032931337809e01,
            -2.625249878460098785e01,
            -4.000000000000000000e01,
        ],
    ]

    idx = sim.idxsim
    ncolst, nmodels, mnames = get_local_data(idx)
    nlay = nlays[idx]

    # make single head array
    ncolt = 0
    for ncol in ncolst:
        ncolt += ncol
    hval = np.zeros((nper, ncolt), dtype=float)
    imid = int(nrow / 2)

    for j in range(nmodels):
        fn = os.path.join(sim.simpath, "{}.hds".format(mnames[j]))
        hobj = flopy.utils.HeadFile(fn)
        times = hobj.get_times()
        ioff = 0
        if j > 0:
            ioff += ncolst[j - 1]
        for n, t in enumerate(times):
            h = hobj.get_data(totim=t)[:, imid, :]
            h = h.reshape((nlay, ncolst[j]))
            ht = np.ones(h.shape[1], dtype=float) * 1e30
            for k in range(nlay):
                v = h[k, :].copy()
                kdx = (ht == 1e30) & (v != -1e30)
                ht[kdx] = v[kdx]
            i1 = ioff + ht.shape[0]
            hval[n, ioff:i1] = ht.copy()
    # fpth = os.path.join(sim.simpath, 'results.dat')
    # np.savetxt(fpth, hval)

    # known results
    if idx < 2:
        h0 = np.array(hdata01lay)
    else:
        h0 = np.array(hdata03lay)

    # calculate maximum absolute error
    diff = hval - h0
    diffmax = np.abs(diff).max()
    dtol = 1e-9
    msg = "maximum absolute maw head difference ({}) ".format(diffmax)

    if diffmax > dtol:
        sim.success = False
        msg += "exceeds {}".format(dtol)
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below
def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        yield test.run_mf6, Simulation(dir, exfunc=eval_hds, idxsim=idx)

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, dir in enumerate(exdirs):
        sim = Simulation(dir, exfunc=eval_hds, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
