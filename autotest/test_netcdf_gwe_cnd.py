"""
NetCDF export test version of test_gwe_cnd.  This test compares
the temperature and input arrays in the the NetCDF file to those
in the FloPy binary output head file and package data objects.
"""

# Imports

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

try:
    import xarray as xa
    import xugrid as xu
except ImportError:
    pytest.skip("xuarray and xugrid not found", allow_module_level=True)

from framework import TestFramework

cases = ["cnd01"]


def build_models(idx, test):
    from test_gwe_cnd import build_models as build

    sim, dummy = build(idx, test)
    sim.tdis.start_date_time = "2041-01-01T00:00:00-05:00"
    gwe = sim.gwe[0]
    gwe.name_file.export_netcdf = "ugrid"
    gwe.dis.export_array_netcdf = True
    gwe.ic.export_array_netcdf = True
    gwe.cnd.export_array_netcdf = True
    return sim, dummy


def check_output(idx, test):
    from test_gwe_cnd import check_output as check

    check(idx, test)

    # read transport results from GWE model
    name = cases[idx]
    gwename = "gwe-" + name

    fpth = os.path.join(test.workspace, f"{gwename}.ucn")
    try:
        # load temperatures
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="TEMPERATURE"
        )
        conc1 = cobj.get_alldata()
    except:
        assert False, f'could not load concentration data from "{fpth}"'

    # Check NetCDF output
    nc_fpth = os.path.join(test.workspace, f"{gwename}.nc")
    ds = xu.open_dataset(nc_fpth)
    xds = ds.ugrid.to_dataset()

    # Compare NetCDF head arrays with binary headfile temperatures
    gwe = test.sims[0].gwe[0]
    dis = getattr(gwe, "dis")
    tdis = getattr(test.sims[0], "tdis")
    nper = getattr(tdis, "nper").data
    nlay = getattr(dis, "nlay").data
    pd = getattr(tdis, "perioddata").array
    print(pd)
    timestep = 0
    for i in range(nper):
        for j in range(int(pd[i][1])):
            rec = cobj.get_data(kstpkper=(j, i))
            for l in range(nlay):
                assert np.allclose(
                    np.array(rec[l]).flatten(),
                    xds[f"temperature_l{l+1}"][timestep, :].data,
                ), f"NetCDF-temperature comparison failure in timestep {timestep+1}"
            timestep += 1

    vlist = [
        "dis_delr",
        "dis_delc",
        "dis_top",
        "dis_botm_l",
        "dis_idomain_l",
        "ic_strt_l",
        "cnd_alh_l",
        "cnd_ath1_l",
        "cnd_ktw_l",
        "cnd_kts_l",
    ]
    layer_vlist = [
        "dis_botm_l",
        "dis_idomain_l",
        "ic_strt_l",
        "cnd_alh_l",
        "cnd_ath1_l",
        "cnd_ktw_l",
        "cnd_kts_l",
    ]

    # Compare NetCDF package input arrays with FloPy arrays
    gwe = test.sims[0].gwe[0]
    for i, var in enumerate(vlist):
        tokens = var.split("_", 1)
        package_name = tokens[0]
        array_name = tokens[1].split("_")[0]
        package = getattr(gwe, package_name)
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


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(cases)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
    )
    test.run()
