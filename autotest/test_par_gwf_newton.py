"""
This tests reuses the simulation data in test_gwf_ifmod_newton.py
and runs it in parallel on three cpus with

cpu 0: 'refmodel'
cpu 1: 'leftmodel'
cpu 2: 'rightmodel'

so we can compare the parallel coupling of 'leftmodel' + 'rightmodel'
with a serial 'refmodel'.

This test also checks that PTC works in parallel.
"""

import pytest

from framework import TestFramework

cases = ["par_newton"]


def build_models(idx, test, netcdf=None):
    from test_gwf_ifmod_newton import build_models as build

    sim, dummy = build(idx, test, netcdf)
    return sim, dummy


def check_output(idx, test):
    from test_gwf_ifmod_newton import check_output as check

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
        ncpus=3,
    )
    test.run()
