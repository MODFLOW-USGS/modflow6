import pytest

from framework import TestFramework

"""
This tests reuses the simulation data in test_gwf_ifmod_rewet.py
and runs it in parallel on three cpus with

cpu 0: 'refmodel'
cpu 1: 'leftmodel'
cpu 2: 'rightmodel'

so we can compare the parallel coupling of 'leftmodel' + 'rightmodel'
with a serial 'refmodel'
"""

ex = ["par_rewet"]


def build_models(idx, test):
    from test_gwf_ifmod_rewet import build_models as build_model_ext

    sim, dummy = build_model_ext(idx, test)
    return sim, dummy


def check_output(test):
    from test_gwf_ifmod_rewet import check_output

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
        build=lambda t: build_models(idx, t),
        check=check_output,
        make_comparison=False,
        parallel=True,
        ncpus=3,
    )
    test.run()
