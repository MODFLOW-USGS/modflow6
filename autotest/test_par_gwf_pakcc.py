import os
import pathlib as pl
from decimal import Decimal

import flopy
import numpy as np
import pytest

from framework import TestFramework

# This tests reuses the simulation data in test_gwf_uzf_gwet
# and runs it in parallel on one and two cpus with
#
# so we can compare the parallel coupling of two models
# with a serial model.
#
# This test also checks that Newton under_relaxation works in parallel.

ex = ["par_uzf_3lay_1p", "par_uzf_3lay_2p"]


def build_model(idx, exdir):
    from test_gwf_uzf_gwet import build_model as build_model_ext

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
    ncpus = 2 if idx == 1 else 1
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda ws: build_model(idx, ws),
        make_comparison=False,
        parallel=True,
        ncpus=ncpus,
    )
    test.run()
