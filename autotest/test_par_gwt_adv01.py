import os
from decimal import Decimal
import pytest
from framework import TestFramework
from simulation import TestSimulation

# This tests reuses the simulation data and config in 
# test_gwt_adv01_gwtgwt.py and runs it in parallel mode.

ex = ["par_adv01a_gwtgwt", "par_adv01b_gwtgwt", "par_adv01c_gwtgwt"]

def build_petsc_db(idx, exdir):
    from test_gwt_adv01_gwtgwt import hclose, ninner
    petsc_db_file = os.path.join(exdir, ".petscrc")
    with open(petsc_db_file, 'w') as petsc_file:
        petsc_file.write("-ksp_type bcgs\n")
        petsc_file.write("-pc_type bjacobi\n")
        petsc_file.write("-sub_pc_type ilu\n")
        petsc_file.write("-sub_pc_factor_levels 2\n")
        petsc_file.write(f"-dvclose {Decimal(hclose):.2E}\n")
        petsc_file.write(f"-nitermax {ninner}\n")


def build_model(idx, exdir):
    from test_gwt_adv01_gwtgwt import build_model as build_model_ext
    build_petsc_db(idx, exdir)
    sim, dummy = build_model_ext(idx, exdir)
    return sim, dummy

def eval_model(test_sim):
    from test_gwt_adv01_gwtgwt import eval_transport
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
            name=name, exe_dict=targets, exfunc=eval_model, 
            idxsim=idx, make_comparison=False,
            parallel=True, ncpus=2,
        ),
        str(function_tmpdir),
    )
