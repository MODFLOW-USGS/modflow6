"""
This tests reuses the simulation data in test_gwf_newton_under_relaxation
and runs it in parallel on one and two cpus with

so we can compare the parallel coupling of two models
with a serial model.

This test also checks that Newton under_relaxation works in parallel.
"""

import os
from decimal import Decimal

import pytest

from framework import TestFramework

cases = ["par_nr_ur01", "par_nr_ur02"]


def build_models(idx, test):
    from test_gwf_newton_under_relaxation import build_models as build

    sim, dummy = build(idx, test)
    return sim, dummy


def check_output(idx, test):
    from test_gwf_newton_under_relaxation import check_output as check

    check(idx, test)


@pytest.mark.parallel
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    ncpus = 2 if idx == 1 else 1
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        compare=None,
        parallel=True,
        ncpus=ncpus,
    )
    test.run()
