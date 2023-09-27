import os
from decimal import Decimal
import pytest
from framework import TestFramework
from simulation import TestSimulation

# This tests reuses the simulation data in test_gwf_newton_under_relaxation
# and runs it in parallel on one and two cpus with
#
# so we can compare the parallel coupling of two models
# with a serial model.
#
# This test also checks that Newton under_relaxation works in parallel.

ex = ["par_nr_ur01", "par_nr_ur02"]


def build_petsc_db(idx, exdir):
    from test_gwf_newton_under_relaxation import hclose, ninner

    petsc_db_file = os.path.join(exdir, ".petscrc")
    with open(petsc_db_file, "w") as petsc_file:
        # PETSc doesn't converge on this example without setting the level
        petsc_file.write("-sub_pc_factor_levels 2\n")


def build_model(idx, exdir):
    from test_gwf_newton_under_relaxation import build_model as build_model_ext

    build_petsc_db(idx, exdir)
    sim, dummy = build_model_ext(idx, exdir)
    return sim, dummy


def eval_model(test_sim):
    from test_gwf_newton_under_relaxation import eval_head as compare_to_ref

    compare_to_ref(test_sim)


@pytest.mark.parallel
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework()
    test.build(build_model, idx, str(function_tmpdir))
    if idx == 1:
        ncpus = 2
    else:
        ncpus = 1
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_model,
            idxsim=idx,
            make_comparison=False,
            parallel=True,
            ncpus=ncpus,
        ),
        str(function_tmpdir),
    )
