"""
This test reuses the simulation data and config in
test_gwt_adv01_gwtgwt.py and runs it in parallel mode.
"""

import pytest

from framework import TestFramework

cases = ["par_adv01a_gwtgwt", "par_adv01b_gwtgwt", "par_adv01c_gwtgwt"]


def build_models(idx, test):
    from test_gwt_adv01_gwtgwt import build_models as build

    sim, dummy = build(idx, test)
    return sim, dummy


def check_output(idx, test):
    from test_gwt_adv01_gwtgwt import check_output as check

    check(idx, test)


@pytest.mark.parallel
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        compare=None,
        parallel=True,
        ncpus=2,
    )
    test.run()
