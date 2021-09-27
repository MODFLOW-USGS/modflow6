import os
import sys
import numpy as np
import shutil
import subprocess

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

import targets

paktest = "sfr"

ex = [
    "sfr_npt01",
    "sfr_npt02",
]
exdirs = [os.path.join("temp", s) for s in ex]

xsect_types = (
    "wide",
    "wide_npoint",
    "slope",
    "slope_r",
    "rectangular",
    "trapezoidal",
    "v",
    "w",
)

# spatial discretization data
nlay, nrow, ncol = 1, 1, 1
delr, delc = 100.0, 100.0
top = 0.0
botm = -10.0
strt = 0.0

# sfr data
nreaches = 10
rlen = 10.0
rwid = 10.0
roughness = 0.001
rbth = 1.0
rhk = 0.0
slope = 0.001
ustrf = 1.0
ndv = 0

np_data = {
    xsect_types[0]: {
        "x": np.array([rwid], dtype=float),
        "d": np.array([0.0], dtype=float),
    },
    xsect_types[1]: {
        "x": np.array([0.0, rwid], dtype=float),
        "d": np.array([0.0, 0.0], dtype=float),
    },
    xsect_types[2]: {
        "x": np.array([0.0, rwid], dtype=float),
        "d": np.array([1.0, 0.0], dtype=float),
    },
    xsect_types[3]: {
        "x": np.array([0.0, rwid], dtype=float),
        "d": np.array([0.0, 1.0], dtype=float),
    },
}

# n-point cross-section functions
def get_wetted_station(x0, x1, d0, d1, d):
    """Get the wetted length in the x-direction"""
    # -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)

    # -- if d is less than or equal to the minimum value the
    #    station length (xlen) is zero
    if d <= dmin:
        x1 = x0
    # -- if d is between dmin and dmax, station length is less
    #    than d1 - d0
    elif d < dmax:
        xlen = x1 - x0
        dlen = d1 - d0
        if abs(dlen) > 0.0:
            slope = xlen / dlen
        else:
            slope = 0.0
        if d0 > d1:
            dx = (d - d1) * slope
            xt = x1 + dx
            xt0 = xt
            xt1 = x1
        else:
            dx = (d - d0) * slope
            xt = x0 + dx
            xt0 = x0
            xt1 = xt
        x0 = xt0
        x1 = xt1
    return x0, x1


def get_wetted_perimeter(x0, x1, d0, d1, d):
    # -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)

    # -- calculate the wetted perimeter for the segment
    xlen = x1 - x0
    if xlen > 0.0:
        if d > dmax:
            dlen = dmax - dmin
        else:
            dlen = d - dmin
    else:
        if d > dmin:
            dlen = min(d, dmax) - dmin
        else:
            dlen = 0.0
    return np.sqrt(xlen ** 2.0 + dlen ** 2.0)


def get_wetted_area(x0, x1, d0, d1, d):
    # -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)

    # -- calculate the wetted area for the segment
    xlen = x1 - x0
    area = 0.0
    if xlen > 0.0:
        # -- add the area above dmax
        if d > dmax:
            area = xlen * (d - dmax)
        # -- add the area below zmax
        if dmax != dmin and d > dmin:
            area += 0.5 * (d - dmin)
    return area


def wetted_area(x, d, v, verbose=False):
    area = 0.0
    if x.shape[0] == 1:
        area = x[0] * v
    else:
        for idx in range(0, x.shape[0] - 1):
            x0, x1 = x[idx], x[idx + 1]
            d0, d1 = d[idx], d[idx + 1]

            # get station data
            x0, x1 = get_wetted_station(x0, x1, d0, d1, v)

            # get wetted area
            a = get_wetted_area(x0, x1, d0, d1, v)
            area += a

            # write to screen
            if verbose:
                print(
                    f"{idx}->{idx + 1} ({x0},{x1}) - "
                    f"perimeter={x1 - x0} - area={a}"
                )

    return area


def wetted_perimeter(x, d, v, verbose=False):
    perimeter = 0.0
    if x.shape[0] == 1:
        perimeter = x[0]
    else:
        for idx in range(0, x.shape[0] - 1):
            x0, x1 = x[idx], x[idx + 1]
            d0, d1 = d[idx], d[idx + 1]

            # get station data
            x0, x1 = get_wetted_station(x0, x1, d0, d1, v)

            # get wetted perimeter
            perimeter += get_wetted_perimeter(x0, x1, d0, d1, v)

            # write to screen
            if verbose:
                print(f"{idx}->{idx + 1} ({x0},{x1}) - perimeter={x1 - x0}")

    return perimeter


def manningsq(x, d, v, roughness=0.01, slope=0.001, conv=1.0):
    perimeter = wetted_perimeter(x, d, v)
    area = wetted_area(x, d, v)
    radius = 0.0
    if perimeter > 0.0:
        radius = area / perimeter
    return conv * area * radius ** 0.666666 * slope ** 0.5 / roughness


def get_depths(
    flows, x, d, roughness=0.01, slope=0.001, conv=1.0, dd=1e-4, verbose=False
):
    if isinstance(flows, float):
        flows = np.array([flows], dtype=float)
    depths = np.zeros(flows.shape, dtype=float)
    for idx, q in enumerate(flows):
        depths[idx] = qtodepth(
            x,
            d,
            q,
            roughness=roughness,
            slope=slope,
            conv=conv,
            dd=dd,
            verbose=False,
        )

    return depths


def qtodepth(
    x, d, q, roughness=0.01, slope=0.001, conv=1.0, dd=1e-4, verbose=False
):
    d0 = 0.0
    q0 = manningsq(x, d, d0, roughness=roughness, slope=slope, conv=conv)
    r = q0 - q

    iter = 0
    if verbose:
        print(f"iteration {iter:>2d} - residual={r}")
    while abs(r) > 1e-12:
        q1 = manningsq(
            x, d, d0 + dd, roughness=roughness, slope=slope, conv=conv
        )
        dq = q1 - q0
        if dq != 0.0:
            derv = dd / (q1 - q0)
        else:
            derv = 0.0
        d0 -= derv * r
        q0 = manningsq(x, d, d0, roughness=roughness, slope=slope, conv=conv)
        r = q0 - q

        iter += 1
        if verbose:
            print(f"iteration {iter:>2d} - residual={r}")
        if iter > 100:
            break
    return d0


#
def get_model(idx, ws):

    xsect_type = xsect_types[idx]

    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = [(11.0, 11, 1.0)]
    ts_times = np.arange(0.0, 12.0, 1.0, dtype=float)
    ts_flows = np.array([1000.0] + [float(q) for q in range(1000, -100, -100)])

    # build MODFLOW 6 files
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="seconds",
        nper=nper,
        perioddata=tdis_rc,
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        length_units="meters",
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
    npf = flopy.mf6.ModflowGwfnpf(gwf)

    # chd files
    # chd data
    spd = [
        [(0, 0, 0), 0.0],
    ]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf, stress_period_data=spd, pname="chd-1"
    )

    # sfr file
    packagedata = []
    for irch in range(nreaches):
        nconn = 1
        if 0 < irch < nreaches - 1:
            nconn += 1
        rp = [
            irch,
            "none",
            rlen,
            rwid,
            slope,
            top,
            rbth,
            rhk,
            roughness,
            nconn,
            ustrf,
            ndv,
        ]
        packagedata.append(rp)

    sfr_tab = f"{name}.sfr.tab"
    if xsect_type != "wide":
        print("adding n-point cross-section data")
        crosssections = []
        for n in range(nreaches):
            crosssections.append([n, sfr_tab])
    else:
        crosssections = None

    connectiondata = []
    for irch in range(nreaches):
        rc = [irch]
        if irch > 0:
            rc.append(irch - 1)
        if irch < nreaches - 1:
            rc.append(-(irch + 1))
        connectiondata.append(rc)

    ts_names = ["inflow"]
    perioddata = [
        [0, "inflow", "inflow"],
    ]
    ts_methods = ["linearend"] * len(ts_names)
    ts_data = []
    for t, q in zip(ts_times, ts_flows):
        ts_data.append((t, q))

    budpth = f"{name}.{paktest}.cbc"
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        print_stage=True,
        print_flows=True,
        print_input=True,
        budget_filerecord=budpth,
        mover=True,
        nreaches=nreaches,
        packagedata=packagedata,
        crosssections=crosssections,
        connectiondata=connectiondata,
        perioddata=perioddata,
        pname="sfr-1",
    )
    fname = f"{name}.sfr.ts"
    sfr.ts.initialize(
        filename=fname,
        timeseries=ts_data,
        time_series_namerecord=ts_names,
        interpolation_methodrecord=ts_methods,
    )
    fname = f"{name}.sfr.obs"
    sfr_obs = {
        f"{fname}.csv": [
            ("inflow", "ext-inflow", (0,)),
            ("outflow", "ext-outflow", (nreaches - 1,)),
            ("stage", "stage", (nreaches - 1,)),
            ("depth", "depth", (nreaches - 1,)),
            ("wp", "wet-perimeter", (nreaches - 1,)),
            ("area", "wet-area", (nreaches - 1,)),
        ]
    }
    sfr.obs.initialize(
        filename=fname, digits=25, print_input=True, continuous=sfr_obs
    )
    if crosssections is not None:
        stations = np_data[xsect_type]["x"] / rwid
        depths = np_data[xsect_type]["d"]
        table = [[x, d] for x, d in zip(stations, depths)]
        flopy.mf6.ModflowUtlsfrtab(gwf, nrow=stations.shape[0], ncol=2,
                                   table=table, filename=sfr_tab)

    # output control
    budpth = f"{name}.cbc"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=budpth,
        printrecord=[
            ("BUDGET", "ALL"),
        ],
        saverecord=[
            ("BUDGET", "ALL"),
        ],
    )

    return sim, None


def eval_npointq(sim):
    idx = sim.idxsim
    name = ex[idx]
    print("evaluating n-point cross-section results..." f"({name})")

    obs_pth = os.path.join(exdirs[idx], f"{name}.sfr.obs.csv")
    obs = flopy.utils.Mf6Obs(obs_pth).get_data()

    assert np.allclose(
        obs["INFLOW"], np.abs(obs["OUTFLOW"])
    ), "inflow not equal to outflow"

    xs_type = xsect_types[idx]
    xs_d = np_data[xs_type]

    d = get_depths(
        obs["INFLOW"],
        xs_d["x"],
        xs_d["d"],
        roughness=roughness,
        slope=slope,
    )

    assert np.allclose(
        obs["STAGE"] - top, d
    ), "sfr stage not equal to calculated stage"

    return


# - No need to change any code below
def build_models():
    for idx, exdir in enumerate(exdirs):
        sim, mc = get_model(idx, exdir)
        sim.write_simulation()
        if mc is not None:
            mc.write_simulation()
    return


def test_mf6model():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, exdir in enumerate(exdirs):
        yield test.run_mf6, Simulation(
            exdir,
            exfunc=eval_npointq,
            idxsim=idx,
            mf6_regression=False,
        )

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    build_models()

    # run the test models
    for idx, exdir in enumerate(exdirs):
        sim = Simulation(
            exdir,
            exfunc=eval_npointq,
            idxsim=idx,
            mf6_regression=False,
        )
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print("standalone run of {}".format(os.path.basename(__file__)))

    # run main routine
    main()
