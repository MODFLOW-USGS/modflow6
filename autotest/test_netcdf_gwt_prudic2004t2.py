"""
NetCDF export test version of test_gwt_prudic2004t2.
"""

import os
import sys

import flopy
import numpy as np
import pytest

from conftest import project_root_path
from framework import TestFramework

try:
    import xarray as xa
    import xugrid as xu
except ImportError:
    pytest.skip("xuarray and xugrid not found", allow_module_level=True)

cases = ["prudic2004t2"]


def build_models(idx, test):
    from test_gwt_prudic2004t2 import build_models as build

    sim, dummy = build(idx, test)
    sim.tdis.start_date_time = "2041-01-01T00:00:00-05:00"
    gwt = sim.gwt[0]
    gwt.name_file.export_netcdf = "ugrid"
    gwt.dis.export_array_netcdf = True
    gwt.ic.export_array_netcdf = True
    gwt.dsp.export_array_netcdf = True
    return sim, dummy


def check_output(idx, test):
    from test_gwt_prudic2004t2 import check_output as check

    check(idx, test)

    # netcdf
    ws = test.workspace
    name = test.name
    gwtname = "gwt_" + name
    nc_fpth = os.path.join(ws, f"{gwtname}.nc")
    ds = xu.open_dataset(nc_fpth)
    xds = ds.ugrid.to_dataset()

    cobj = flopy.utils.HeadFile(
        os.path.join(ws, f"{gwtname}.ucn"),
        precision="double",
        text="CONCENTRATION",
    )

    # Compare NetCDF head arrays with binary headfile concentrations
    gwt = test.sims[0].gwt[0]
    dis = getattr(gwt, "dis")
    tdis = getattr(test.sims[0], "tdis")
    nper = getattr(tdis, "nper").data
    nlay = getattr(dis, "nlay").data
    pd = getattr(tdis, "perioddata").array
    timestep = 0
    for i in range(nper):
        for j in range(pd[i][1]):
            rec = cobj.get_data(kstpkper=(j, i))
            for l in range(nlay):
                assert np.allclose(
                    np.array(rec[l]).flatten(),
                    xds[f"concentration_l{l+1}"][timestep, :]
                    .fillna(1.00000000e30)
                    .data,
                ), f"NetCDF-concentration comparison failure in timestep {timestep+1}"
            timestep += 1

    vlist = [
        "dis_delr",
        "dis_delc",
        "dis_top",
        "dis_botm_l",
        "dis_idomain_l",
        "ic_strt_l",
        "dsp_alh_l",
        "dsp_ath1_l",
        "dsp_atv_l",
    ]
    layer_vlist = [
        "dis_botm_l",
        "dis_idomain_l",
        "ic_strt_l",
        "dsp_alh_l",
        "dsp_ath1_l",
        "dsp_atv_l",
    ]

    # Compare NetCDF package input arrays with FloPy arrays
    gwt = test.sims[0].gwt[0]
    for i, var in enumerate(vlist):
        tokens = var.split("_", 1)
        package_name = tokens[0]
        array_name = tokens[1].split("_")[0]
        package = getattr(gwt, package_name)
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


@pytest.mark.slow
@pytest.mark.netcdf
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
