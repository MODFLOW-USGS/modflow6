import os
from decimal import Decimal
import pytest
from framework import TestFramework

# This tests reuses the simulation data and config in 
# test_gwt_dsp01_gwtgwt.py and runs it in parallel mode.

ex = ["par_dsp01_gwtgwt"]

def build_model(idx, exdir):
    from test_gwt_dsp01_gwtgwt import build_model as build_model_ext
    sim, dummy = build_model_ext(idx, exdir)
    return sim, dummy

def eval_model(test_sim):
    from test_gwt_dsp01_gwtgwt import eval_transport
    eval_transport(test_sim)    

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
        build=lambda ws: build_model(idx, ws),
        check=eval_model, 
        make_comparison=False,
        parallel=True,
        ncpus=2,
    )
    test.run()

