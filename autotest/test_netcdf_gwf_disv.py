"""
Test of GWF DISV Package.  Use the flopy disv tool to create
a simple regular grid example, but using DISV instead of DIS.
Use a large offset for x and y vertices to ensure that the area
calculation in MODFLOW 6 is correct.  For the second case, set
one of the cells inactive and test to make sure connectivity
in binary grid file is correct.
"""

import os

import flopy
import numpy as np
import pytest
from flopy.utils.gridutil import get_disv_kwargs

from framework import TestFramework

try:
    import xarray as xa
    import xugrid as xu
except ImportError:
    pytest.skip("xuarray and xugrid not found", allow_module_level=True)

cases = ["disv01a_ncf", "disv01b_ncf"]

wkt = (
    'PROJCS["NAD83 / UTM zone 18N", '
    'GEOGCS["NAD83", '
    'DATUM["North_American_Datum_1983", '
    'SPHEROID["GRS 1980",6378137,298.257222101], '
    'TOWGS84[0,0,0,0,0,0,0]], '
    'PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]], '
    'UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]], '
    'AUTHORITY["EPSG","4269"]], '
    'PROJECTION["Transverse_Mercator"], '
    'PARAMETER["latitude_of_origin",0], '
    'PARAMETER["central_meridian",-75], '
    'PARAMETER["scale_factor",0.9996], '
    'PARAMETER["false_easting",500000], '
    'PARAMETER["false_northing",0], '
    'UNIT["metre",1,AUTHORITY["EPSG","9001"]], '
    'AXIS["Easting",EAST], '
    'AXIS["Northing",NORTH], '
    'AUTHORITY["EPSG","26918"]]'
)


def build_models(idx, test):
    name = cases[idx]
    ws = test.workspace
    nlay = 3
    nrow = 3
    ncol = 3
    delr = 10.0
    delc = 10.0
    top = 0
    botm = [-10, -20, -30]
    xoff = 100000000.0
    yoff = 100000000.0
    disvkwargs = get_disv_kwargs(
        nlay,
        nrow,
        ncol,
        delr,
        delc,
        top,
        botm,
        xoff,
        yoff,
    )
    disvkwargs["export_array_netcdf"] = True
    if idx == 1:
        # for the second test, set one cell to idomain = 0
        idomain = np.ones((nlay, nrow * ncol), dtype=int)
        idomain[0, 1] = 0
        disvkwargs["idomain"] = idomain

    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", start_date_time="2041-01-01T00:00:00-05:00"
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, export_netcdf="ugrid")
    ims = flopy.mf6.ModflowIms(sim, print_option="SUMMARY")
    disv = flopy.mf6.ModflowGwfdisv(gwf, **disvkwargs)
    # netcdf configuration
    ncf = flopy.mf6.ModflowUtlncf(disv, ogc_wkt=wkt, filename=f"{name}.disv.ncf")
    ic = flopy.mf6.ModflowGwfic(gwf, export_array_netcdf=True, strt=0.0)
    npf = flopy.mf6.ModflowGwfnpf(gwf, export_array_netcdf=True)
    spd = {0: [[(0, 0), 1.0], [(0, nrow * ncol - 1), 0.0]]}
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(gwf, stress_period_data=spd)
    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        # budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        # headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        # saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        saverecord=[
            ("HEAD", "ALL"),
        ],
        # printrecord=[("HEAD", "LAST"), ("BUDGET", "ALL")],
    )
    return sim, None


def check_output(idx, test):
    name = test.name

    fname = os.path.join(test.workspace, name + ".disv.grb")
    grbobj = flopy.mf6.utils.MfGrdFile(fname)
    ncpl = grbobj._datadict["NCPL"]
    ia = grbobj._datadict["IA"]
    ja = grbobj._datadict["JA"]

    if idx == 1:
        # assert ncpl == disvkwargs["ncpl"]
        assert np.array_equal(ia[0:4], np.array([1, 4, 4, 7]))
        assert np.array_equal(ja[:6], np.array([1, 4, 10, 3, 6, 12]))
        assert ia[-1] == 127
        assert ia.shape[0] == 28, "ia should have size of 28"
        assert ja.shape[0] == 126, "ja should have size of 126"

    # Check NetCDF output
    nc_fpth = os.path.join(test.workspace, name + ".nc")
    ds = xu.open_dataset(nc_fpth)
    xds = ds.ugrid.to_dataset()

    hds_fpth = os.path.join(test.workspace, name + ".hds")
    hds = flopy.utils.HeadFile(hds_fpth, precision="double")

    # Compare NetCDF head arrays with binary headfile
    gwf = test.sims[0].gwf[0]
    disv = getattr(gwf, "disv")
    tdis = getattr(test.sims[0], "tdis")
    nper = getattr(tdis, "nper").data
    nlay = getattr(disv, "nlay").data
    pd = getattr(tdis, "perioddata").array
    timestep = 0
    for i in range(nper):
        for j in range(pd[i][1]):
            rec = hds.get_data(kstpkper=(j, i))
            for l in range(nlay):
                assert np.allclose(
                    np.array(rec[l]).flatten(),
                    #xds[f"head_l{l+1}"][timestep, :].data,
                    xds[f"head_l{l+1}"][timestep, :].fillna(1.00000000e+30).data,
                ), f"NetCDF-Headfile comparison failure in timestep {timestep+1}"
            timestep += 1

    # NetCDF variables
    vlist = [
        "disv_top",
        "disv_botm_l",
        "npf_icelltype_l",
        "npf_k_l",
        "ic_strt_l",
    ]

    # NetCDF layered variable basenames
    layer_vlist = [
        "disv_botm_l",
        "npf_icelltype_l",
        "npf_k_l",
        "ic_strt_l",
    ]

    # Compare NetCDF package input arrays with FloPy arrays
    for i, var in enumerate(vlist):
        tokens = var.split("_", 1)
        package_name = tokens[0]
        array_name = tokens[1].split("_")[0]
        package = getattr(gwf, package_name)
        b = getattr(package, array_name).array
        if var in layer_vlist:
            for l in range(nlay):
                assert np.allclose(
                    np.array(b[l]).flatten(), xds[f"{var}{l+1}"].data
                ), f"NetCDF input array comparison failure, variable={var}{l+1}"
        else:
            assert np.allclose(
                np.array(b).flatten(), xds[var].data
            ), f"NetCDF input array comparison failure, variable={var}"


@pytest.mark.netcdf
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        compare=None,
    )
    test.run()
