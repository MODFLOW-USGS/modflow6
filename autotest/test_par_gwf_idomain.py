import os
from decimal import Decimal
import pytest
from framework import TestFramework
from simulation import TestSimulation

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

def build_petsc_db(idx, exdir):
    from test_gwf_ifmod_idomain import hclose_check, max_inner_it
    petsc_db_file = os.path.join(exdir, ".petscrc")
    with open(petsc_db_file, 'w') as petsc_file:
        petsc_file.write("-ksp_type bcgs\n")
        petsc_file.write("-pc_type bjacobi\n")
        petsc_file.write("-sub_pc_type ilu\n")
        petsc_file.write("-sub_pc_factor_levels 2\n")
        petsc_file.write(f"-dvclose {Decimal(hclose_check):.2E}\n")
        petsc_file.write(f"-nitermax {max_inner_it}\n")

def build_model(idx, exdir):
    from test_gwf_ifmod_idomain import build_model as build_model_ext
    build_petsc_db(idx, exdir)
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
    test = TestFramework()
    test.build(build_model, idx, str(function_tmpdir))
    test.run(
        TestSimulation(
            name=name, exe_dict=targets, exfunc=eval_model, 
            idxsim=0, make_comparison=False,
            parallel=True, ncpus=3,
        ),
        str(function_tmpdir),
    )
