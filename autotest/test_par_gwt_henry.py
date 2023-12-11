import pytest
from framework import TestFramework

"""
This test reuses the simulation data and config in
test_gwt_henry_gwtgwt.py and runs it in parallel mode.
"""

ex = ["par-henry-ups", "par-henry-cen", "par-henry-tvd"]


def build_models(idx, test):
    from test_gwt_henry_gwtgwt import build_models as build

    sim, dummy = build(idx, test)
    return sim, dummy


def check_output(test):
    from test_gwt_henry_gwtgwt import check_output as check

    check(test)


@pytest.mark.parallel
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=check_output,
        compare=None,
        parallel=True,
        ncpus=2,
    )
    test.run()
