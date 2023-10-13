import os
from decimal import Decimal
import pytest
from framework import TestFramework
from simulation import TestSimulation

# This tests reuses the simulation data and config in
# test_gwt_henry_gwtgwt.py and runs it in parallel mode.

ex = ["par-henry-ups", "par-henry-cen", "par-henry-tvd"]


def build_model(idx, exdir):
    from test_gwt_henry_gwtgwt import build_model as build_model_ext

    sim, dummy = build_model_ext(idx, exdir)
    return sim, dummy


def eval_model(test_sim):
    from test_gwt_henry_gwtgwt import eval_transport

    eval_transport(test_sim)


@pytest.mark.parallel
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework()
    test.build(build_model, idx, str(function_tmpdir))
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_model,
            idxsim=idx,
            make_comparison=False,
            parallel=True,
            ncpus=2,
        ),
        str(function_tmpdir),
    )
