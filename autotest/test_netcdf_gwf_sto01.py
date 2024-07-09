"""
NetCDF export test version of test_gwf_sto01.
"""

import os

import flopy
import numpy as np
import pytest
from conftest import try_get_target
from framework import TestFramework

try:
    import xarray as xa
    import xugrid as xu
except ImportError:
    pytest.skip("xuarray and xugrid not found", allow_module_level=True)

cases = ["gwf_sto01"]
htol = [None for _ in range(len(cases))]

wkt = (
    'PROJCS["NAD83 / UTM zone 18N", '
    'GEOGCS["NAD83", '
    'DATUM["North_American_Datum_1983", '
    'SPHEROID["GRS 1980",6378137,298.257222101], '
    "TOWGS84[0,0,0,0,0,0,0]], "
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
    from test_gwf_sto01 import build_models as build

    sim, dummy = build(idx, test)
    sim.tdis.start_date_time = "2041-01-01T00:00:00-05:00"
    gwf = sim.gwf[0]
    gwf.name_file.export_netcdf = "ugrid"
    gwf.dis.export_array_netcdf = True
    gwf.ic.export_array_netcdf = True
    gwf.npf.export_array_netcdf = True

    name = cases[idx]

    # netcdf config
    ncf = flopy.mf6.ModflowUtlncf(
        gwf.dis, ogc_wkt=wkt, filename=f"{name}.dis.ncf"
    )
    return sim, dummy


def check_output(idx, test):
    from test_gwf_sto01 import check_output as check

    check(idx, test)

    # Check NetCDF output
    nc_fname = f"{os.path.basename(test.name)}.nc"
    nc_fpth = os.path.join(test.workspace, nc_fname)
    ds = xu.open_dataset(nc_fpth)
    xds = ds.ugrid.to_dataset()

    hds_fpth = os.path.join(
        test.workspace, f"{os.path.basename(test.name)}.hds"
    )
    hds = flopy.utils.HeadFile(hds_fpth, precision="double")

    gwf = test.sims[0].gwf[0]
    dis = getattr(gwf, "dis")
    tdis = getattr(test.sims[0], "tdis")
    nper = getattr(tdis, "nper").data
    nstp = [1] + [6 for _ in range(nper - 1)]
    nlay = getattr(dis, "nlay").data

    # Compare NetCDF head arrays with binary headfile
    timestep = 0
    for i in range(nper):
        for j in range(nstp[i]):
            rec = hds.get_data(kstpkper=(j, i))
            for l in range(nlay):
                assert np.allclose(
                    np.array(rec[l]).flatten(),
                    xds[f"head_l{l+1}"][timestep, :].data,
                ), f"NetCDF-Headfile comparison failure in timestep {timestep+1}"
            timestep += 1

    vlist = [
        "dis_delr",
        "dis_delc",
        "dis_top",
        "dis_botm_l",
        "npf_icelltype_l",  # int
        "npf_k_l",
        "npf_k33_l",
        "ic_strt_l",
    ]
    layer_vlist = [
        "dis_botm_l",
        "npf_icelltype_l",
        "npf_k_l",
        "npf_k33_l",
        "ic_strt_l",
    ]

    # Compare NetCDF package input arrays with FloPy arrays
    gwf = test.sims[0].gwf[0]
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
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
        htol=htol[idx],
    )
    test.run()
