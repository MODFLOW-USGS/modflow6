import os
from decimal import Decimal
import pytest
from framework import TestFramework

# This tests reuses the simulation data in test_gwf_ifmod_idomain.py
# and runs it in parallel on three cpus with
#
# cpu 0: 'refmodel'
# cpu 1: 'leftmodel'
# cpu 2: 'rightmodel'
#
# so we can compare the parallel coupling of 'leftmodel' + 'rightmodel'
# with a serial 'refmodel'

ex = ["par_idomain"]

def build_model(idx, exdir):
    from test_gwf_ifmod_idomain import build_model as build_model_ext
    sim, dummy = build_model_ext(idx, exdir)
    return sim, dummy

def eval_model(test_sim):
    from test_gwf_ifmod_idomain import compare_to_ref
    compare_to_ref(test_sim)    

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
        ncpus=3,
    )
    test.run()
