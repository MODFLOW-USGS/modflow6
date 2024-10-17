"""
NetCDF export test version of test_gwf_sto01.
"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework
from test_gwf_sto01 import cases

try:
    import xarray as xa
    import xugrid as xu
except ImportError:
    pytest.skip("xarray and xugrid not found", allow_module_level=True)

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


def build_models(idx, test, export, gridded_input):
    from test_gwf_sto01 import build_models as build

    sim, dummy = build(idx, test)
    sim.tdis.start_date_time = "2041-01-01T00:00:00-05:00"
    gwf = sim.gwf[0]
    gwf.name_file.export_netcdf = export
    gwf.dis.export_array_netcdf = True
    gwf.ic.export_array_netcdf = True
    gwf.npf.export_array_netcdf = True
    gwf.sto.export_array_netcdf = True

    name = cases[idx]

    # netcdf config
    ncf = flopy.mf6.ModflowUtlncf(
        gwf.dis,
        ogc_wkt=wkt,
        filename=f"{name}.dis.ncf",
    )

    return sim, dummy


def check_output(idx, test, export, gridded_input):
    from test_gwf_sto01 import check_output as check

    if gridded_input == "netcdf":
        # re-run the simulation with model netcdf input
        input_fname = "gwf_sto01.nc"
        nc_fname = f"gwf_sto01.{export}.nc"
        os.rename(test.workspace / input_fname, test.workspace / nc_fname)

        with open(test.workspace / "gwf_sto01.nam", "w") as f:
            f.write("BEGIN options\n")
            f.write("  SAVE_FLOWS\n")
            f.write("  NEWTON\n")
            f.write(f"  EXPORT_NETCDF {export}\n")
            f.write(f"  NETCDF  FILEIN gwf_sto01.{export}.nc\n")
            f.write("END options\n\n")
            f.write("BEGIN packages\n")
            f.write("  DIS6  gwf_sto01.dis  dis\n")
            f.write("  IC6  gwf_sto01.ic  ic\n")
            f.write("  NPF6  gwf_sto01.npf  npf\n")
            f.write("  STO6  gwf_sto01.sto  sto\n")
            f.write("  RCH6  gwf_sto01.rcha  rcha_0\n")
            f.write("  WEL6  gwf_sto01.wel  wel_0\n")
            f.write("  CHD6  gwf_sto01.chd  chd_0\n")
            f.write("  OC6  gwf_sto01.oc  oc\n")
            f.write("END packages\n")

        with open(test.workspace / "gwf_sto01.dis", "w") as f:
            f.write("BEGIN options\n")
            f.write("  EXPORT_ARRAY_NETCDF\n")
            f.write("  NCF6  FILEIN  gwf_sto01.dis.ncf\n")
            f.write("END options\n\n")
            f.write("BEGIN dimensions\n")
            f.write("  NLAY  3\n")
            f.write("  NROW  10\n")
            f.write("  NCOL  10\n")
            f.write("END dimensions\n\n")
            f.write("BEGIN griddata\n")
            f.write("  delr NETCDF\n")
            f.write("  delc NETCDF\n")
            f.write("  top NETCDF\n")
            f.write("  botm NETCDF\n")
            f.write("END griddata\n\n")

        with open(test.workspace / "gwf_sto01.ic", "w") as f:
            f.write("BEGIN options\n")
            f.write("  EXPORT_ARRAY_NETCDF\n")
            f.write("END options\n\n")
            f.write("BEGIN griddata\n")
            f.write("  strt NETCDF\n")
            f.write("END griddata\n")

        with open(test.workspace / "gwf_sto01.npf", "w") as f:
            f.write("BEGIN options\n")
            f.write("  EXPORT_ARRAY_NETCDF\n")
            f.write("END options\n\n")
            f.write("BEGIN griddata\n")
            f.write("  icelltype  NETCDF\n")
            f.write("  k  NETCDF\n")
            f.write("  k33  NETCDF\n")
            f.write("END griddata\n")

        with open(test.workspace / "gwf_sto01.sto", "w") as f:
            f.write("BEGIN options\n")
            f.write("  EXPORT_ARRAY_NETCDF\n")
            f.write("END options\n\n")
            f.write("BEGIN griddata\n")
            f.write("  iconvert  NETCDF\n")
            f.write("  ss  NETCDF\n")
            f.write("  sy  NETCDF\n")
            f.write("END griddata\n\n")
            f.write("BEGIN period 1\n")
            f.write("  STEADY-STATE\n")
            f.write("END period 1\n\n")
            f.write("BEGIN period 2\n")
            f.write("  TRANSIENT\n")
            f.write("END period 2\n")

        success, buff = flopy.run_model(
            test.targets["mf6"],
            test.workspace / "mfsim.nam",
            model_ws=test.workspace,
            report=True,
        )

        assert success
        test.success = success

    check(idx, test)

    # Check NetCDF output
    nc_fname = f"{os.path.basename(test.name)}.nc"
    nc_fpth = os.path.join(test.workspace, nc_fname)
    if export == "ugrid":
        ds = xu.open_dataset(nc_fpth)
        xds = ds.ugrid.to_dataset()
    elif export == "structured":
        xds = xa.open_dataset(nc_fpth)

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
            if export == "ugrid":
                for l in range(nlay):
                    assert np.allclose(
                        np.array(rec[l]).flatten(),
                        xds[f"head_l{l+1}"][timestep, :].data,
                    ), f"NetCDF-Headfile comparison failure in timestep {timestep+1}"
                timestep += 1
            elif export == "structured":
                assert np.allclose(
                    # np.array(rec).flatten(),
                    np.array(rec),
                    xds["head"][timestep, :].data,
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
        "sto_iconvert_l",
        "sto_ss_l",
        "sto_sy_l",
    ]

    # Compare NetCDF package input arrays with FloPy arrays
    gwf = test.sims[0].gwf[0]
    for i, var in enumerate(vlist):
        tokens = var.split("_", 1)
        package_name = tokens[0]
        array_name = tokens[1].split("_")[0]
        package = getattr(gwf, package_name)
        b = getattr(package, array_name).array
        if export == "ugrid":
            if var.endswith("_l"):
                for l in range(nlay):
                    assert np.allclose(
                        np.array(b[l]).flatten(), xds[f"{var}{l+1}"].data
                    ), f"NetCDF input array comparison failure, variable={var}{l+1}"
            else:
                assert np.allclose(
                    np.array(b).flatten(), xds[var].data
                ), f"NetCDF input array comparison failure, variable={var}"
        elif export == "structured":
            var = var.replace("_l", "")
            assert np.allclose(
                # np.array(b).flatten(), xds[var].data
                np.array(b),
                xds[var].data,
            ), f"NetCDF input array comparison failure, variable={var}"


@pytest.mark.netcdf
@pytest.mark.parametrize("idx, name", enumerate(cases))
@pytest.mark.parametrize("export", ["ugrid", "structured"])
@pytest.mark.parametrize("gridded_input", ["ascii", "netcdf"])
def test_mf6model(idx, name, function_tmpdir, targets, export, gridded_input):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t, export, gridded_input),
        check=lambda t: check_output(idx, t, export, gridded_input),
        targets=targets,
        htol=htol[idx],
    )
    test.run()
