import os

import flopy
import numpy as np
import pytest
from framework import TestFramework

# Test for parallel MODFLOW running on two cpus.
# It contains two coupled models with
#
# 1d:  (nlay,nrow,ncol) = (1,1,5),
#
# constant head boundaries left=1.0, right=10.0.
# The result should be a uniform flow field.

ex = ["par_gwf_csv"]
dis_shape = [(1, 1, 5)]

# global convenience...
name_left = "leftmodel"
name_right = "rightmodel"


def update_ims(idx, ims):
    from test_par_gwf01 import hclose, rclose, nouter, ninner

    name = ex[idx]
    ims.csv_outer_output_filerecord.set_data(f"{name}.outer.csv")
    ims.csv_inner_output_filerecord.set_data(f"{name}.inner.csv")
    return


def build_model(idx, exdir):
    from test_par_gwf01 import get_model as get_model_ext
    from test_par_gwf01 import ex as ex_ext

    sim = get_model_ext(idx, exdir)
    update_ims(idx, sim.get_solution_package(f"{ex_ext[idx]}.ims"))
    return sim, None


def eval_model(sim):
    from test_par_gwf01 import eval_model as eval_model_ext

    eval_model_ext(sim)


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
