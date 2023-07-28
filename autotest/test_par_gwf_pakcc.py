import os
import pathlib as pl
from decimal import Decimal

import flopy
import numpy as np
import pytest

from framework import TestFramework
from simulation import TestSimulation

# This tests reuses the simulation data in test_gwf_uzf_gwet
# and runs it in parallel on one and two cpus with
#
# so we can compare the parallel coupling of two models
# with a serial model.
#
# This test also checks that Newton under_relaxation works in parallel.

ex = ["par_uzf_3lay_1p", "par_uzf_3lay_2p"]


def build_petsc_db(idx, exdir):
    from test_gwf_uzf_gwet import hclose, ninner

    petsc_db_file = os.path.join(exdir, ".petscrc")
    with open(petsc_db_file, "w") as petsc_file:
        petsc_file.write("-ksp_type bicg\n")
        petsc_file.write("-pc_type bjacobi\n")
        petsc_file.write("-sub_pc_type ilu\n")
        petsc_file.write("-sub_pc_factor_levels 2\n")
        petsc_file.write(f"-dvclose {Decimal(hclose):.2E}\n")
        petsc_file.write(f"-nitermax {ninner}\n")
        petsc_file.write("-options_left no\n")


def build_model(idx, exdir):
    from test_gwf_uzf_gwet import build_model as build_model_ext

    build_petsc_db(idx, exdir)
    sim, dummy = build_model_ext(idx, exdir)
    if idx == 1:
        sim.set_sim_path(exdir / "working")
        sim.write_simulation(silent=True)
        mfsplit = flopy.mf6.utils.Mf6Splitter(sim)
        split_array = np.zeros((10), dtype=int)
        split_array[5:] = 1
        new_sim = mfsplit.split_model(split_array)
        new_sim.set_sim_path(exdir)
        mfsplit.save_node_mapping(pl.Path(f"{exdir}/mapping.json"))
        return new_sim, None
    else:
        return sim, dummy


@pytest.mark.parallel
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework()
    test.build(build_model, idx, function_tmpdir)
    if idx == 1:
        ncpus = 2
    else:
        ncpus = 1
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            idxsim=idx,
            make_comparison=False,
            parallel=True,
            ncpus=ncpus,
        ),
        str(function_tmpdir),
    )
