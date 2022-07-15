"""
Test for the interface model approach, when running
with a GWF-GWF exchange with horizontal _and_ vertical
connections.

layer 1:

        1 1 1 1 1 1 1
        1 1 1 1 1 1 1
        1 1 0 0 0 1 1
(H=1.0) 1 1 0 0 0 1 1 (H=0.0)   with the 9x9 refined grid inside
        1 1 0 0 0 1 1
        1 1 1 1 1 1 1
        1 1 1 1 1 1 1

layer 2/3:

        1 1 1 1 1 1 1
        1 1 1 1 1 1 1
        1 1 1 1 1 1 1
(H=1.0) 1 1 1 1 1 1 1 (H=0.0)
        1 1 1 1 1 1 1
        1 1 1 1 1 1 1
        1 1 1 1 1 1 1


The exchange will have XT3D enabled so the head values in
the child model should match the theory. In this case we
just assert that they are equal for each column, something
that is clearly not through when simulating without XT3D.

"""
import os

import numpy as np
import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from flopy.utils.lgrutil import Lgr

from framework import testing_framework
from simulation import Simulation

ex = ["ifmod_vert"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

parent_name = "parent"
child_name = "child"
h_left = 1.0
h_right = 0.0
delr = 100.0
delc = 100.0
k11 = 1.0
k33 = 1.0


def get_model(idx, dir):
    global child_domain
    global hclose

    name = ex[idx]

    # tdis period data
    nper = 1
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((1.0, 1, 1))

    # solver data
    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-12, 1e-3, 0.97
    h_start = 1.0

    # dis
    nlay, nrow, ncol = 3, 7, 7
    nlayc = 1

    row_s, row_e = 3, 5
    col_s, col_e = 3, 5

    ref_fct = 3
    nrowc = ref_fct * ((row_e - row_s) + 1)
    ncolc = ref_fct * ((col_e - col_s) + 1)

    idomain = np.ones((nlay, nrow, ncol))
    idomain[0, row_s - 1 : row_e, col_s - 1 : col_e] = 0

    delrc = delr / ref_fct
    delcc = delc / ref_fct
    tops = [0.0, -100.0, -200.0, -300.0]

    xoriginc = 2 * delr
    yoriginc = 2 * delc

    # boundary stress period data
    left_chd = [
        [(ilay, irow, 0), h_left]
        for ilay in range(nlay)
        for irow in range(nrow)
    ]
    right_chd = [
        [(ilay, irow, ncol - 1), h_right]
        for ilay in range(nlay)
        for irow in range(nrow)
    ]
    chd_data = left_chd + right_chd
    chd_spd = {0: chd_data}

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        memory_print_option="ALL",
    )

    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        relaxation_factor=relax,
    )

    # The parent model:
    gwf = flopy.mf6.ModflowGwf(sim, modelname=parent_name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=tops[0],
        botm=tops[1:],
        idomain=idomain,
    )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        xt3doptions=False,
        save_flows=True,
        icelltype=0,
        k=k11,
        k33=k33,
    )
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{parent_name}.hds",
        budget_filerecord=f"{parent_name}.cbc",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # The child model:
    gwfc = flopy.mf6.ModflowGwf(sim, modelname=child_name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(
        gwfc,
        nlay=nlayc,
        nrow=nrowc,
        ncol=ncolc,
        delr=delrc,
        delc=delcc,
        top=tops[0],
        botm=tops[1],
        xorigin=xoriginc,
        yorigin=yoriginc,
    )
    ic = flopy.mf6.ModflowGwfic(gwfc, strt=h_start)
    npf = flopy.mf6.ModflowGwfnpf(
        gwfc,
        save_specific_discharge=True,
        xt3doptions=False,
        save_flows=True,
        icelltype=0,
        k=k11,
        k33=k33,
    )
    oc = flopy.mf6.ModflowGwfoc(
        gwfc,
        head_filerecord=f"{child_name}.hds",
        budget_filerecord=f"{child_name}.cbc",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # LGR:
    nrowp = gwf.dis.nrow.get_data()
    ncolp = gwf.dis.ncol.get_data()
    delrp = gwf.dis.delr.array
    delcp = gwf.dis.delc.array
    topp = gwf.dis.top.array
    botmp = gwf.dis.botm.array
    idomainp = gwf.dis.idomain.array

    lgr = Lgr(
        3,
        nrowp,
        ncolp,
        delrp,
        delcp,
        topp,
        botmp,
        idomainp,
        ncpp=ref_fct,
        ncppl=[1, 0, 0],
    )

    exgdata = lgr.get_exchange_data(angldegx=True, cdist=True)

    gwfgwf = flopy.mf6.ModflowGwfgwf(
        sim,
        exgtype="GWF6-GWF6",
        nexg=len(exgdata),
        exgmnamea=parent_name,
        exgmnameb=child_name,
        exchangedata=exgdata,
        xt3d=True,
        print_flows=True,
        auxiliary=["ANGLDEGX", "CDIST"],
        dev_interfacemodel_on=True,
    )

    return sim


def build_model(idx, exdir):
    sim = get_model(idx, exdir)
    return sim, None


def eval_heads(sim):
    print("comparing heads  for child model to analytical result...")

    fpth = os.path.join(sim.simpath, f"{child_name}.hds")
    hds_c = flopy.utils.HeadFile(fpth)
    heads_c = hds_c.get_data()

    fpth = os.path.join(sim.simpath, f"{child_name}.dis.grb")
    grb_c = flopy.mf6.utils.MfGrdFile(fpth)

    # (note that without XT3D on the exchange, the 'error'
    # is of order 1e-3!!)
    deviations = np.array(
        [np.std(heads_c[0, :, icol]) for icol in range(grb_c.ncol)]
    )
    assert np.any(
        deviations < 1e-12
    ), "head values deviate too much from theory"

    # check flowja residual
    for mname in [parent_name, child_name]:
        print(f"Checking flowja residual for model {mname}")

        fpth = os.path.join(sim.simpath, f"{mname}.dis.grb")
        grb = flopy.mf6.utils.MfGrdFile(fpth)
        ia = grb._datadict["IA"] - 1

        fpth = os.path.join(sim.simpath, f"{mname}.cbc")
        assert os.path.isfile(fpth)
        cbb = flopy.utils.CellBudgetFile(fpth, precision="double")
        flow_ja_face = cbb.get_data(idx=0)
        assert (
            len(flow_ja_face) > 0
        ), "Could not check residuals as flow-ja-face could not be found"

        for fjf in flow_ja_face:
            fjf = fjf.flatten()
            res = fjf[ia[:-1]]
            errmsg = f"min or max residual too large {res.min()} {res.max()}"
            assert np.allclose(res, 0.0, atol=1.0e-6), errmsg

    return


@pytest.mark.parametrize(
    "idx, exdir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, exdir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_model, idx, exdir)

    # run the test model
    test.run_mf6(Simulation(exdir, exfunc=eval_heads, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test models
    for idx, exdir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, exdir)

        sim = Simulation(exdir, exfunc=eval_heads, idxsim=idx)
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
