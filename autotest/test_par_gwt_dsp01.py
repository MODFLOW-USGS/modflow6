import os
from decimal import Decimal

import pytest

from framework import TestFramework

# This tests reuses the simulation data and config in
# test_gwt_dsp01_gwtgwt.py and runs it in parallel mode.

ex = ["par_dsp01_gwtgwt"]


def build_model(idx, test):
    from test_gwt_dsp01_gwtgwt import build_models as build_model_ext
    sim, dummy = build_model_ext(idx, test)
    return sim, dummy


def check_transport(test):
    from test_gwt_dsp01_gwtgwt import check_output
    check_output(test)


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
        build=lambda t: build_model(idx, t),
        check=check_transport,
        make_comparison=False,
        parallel=True,
        ncpus=2,
    )
    test.run()
