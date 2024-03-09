"""
This test reuses the simulation data and config in
test_gwt_dsp01_gwtgwt.py and runs it in parallel mode.
"""

import pytest

from framework import TestFramework

cases = ["par_dsp01_gwtgwt"]


def build_models(idx, test, netcdf=None):
    from test_gwt_dsp01_gwtgwt import build_models as build

    sim, dummy = build(idx, test, netcdf)
    return sim, dummy


def check_output(idx, test):
    from test_gwt_dsp01_gwtgwt import check_output as check

    check(idx, test)


@pytest.mark.parallel
@pytest.mark.parametrize("idx, name", enumerate(cases))
@pytest.mark.parametrize(
    "netcdf", [0, pytest.param(1, marks=pytest.mark.netcdf)]
)
def test_mf6model(idx, name, function_tmpdir, targets, netcdf):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t, netcdf),
        check=lambda t: check_output(idx, t),
        compare=None,
        parallel=True,
        netcdf=netcdf,
        ncpus=2,
    )
    test.run()
