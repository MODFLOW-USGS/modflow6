import flopy
import pytest

from framework import TestFramework

"""
Test for parallel MODFLOW running on two cpus.
It contains two coupled models with

1d:  (nlay,nrow,ncol) = (1,1,5),

constant head boundaries left=1.0, right=10.0.
The result should be a uniform flow field.
"""

cases = ["par_gwf_csv"]
dis_shape = [(1, 1, 5)]

# global convenience...
name_left = "leftmodel"
name_right = "rightmodel"


def update_ims(idx, ims):
    from test_par_gwf01 import hclose, ninner, nouter, rclose

    name = cases[idx]
    ims.csv_outer_output_filerecord.set_data(f"{name}.outer.csv")
    ims.csv_inner_output_filerecord.set_data(f"{name}.inner.csv")
    return


def build_models(idx, test, netcdf=None):
    from test_par_gwf01 import cases as ex_ext
    from test_par_gwf01 import get_model

    sim = get_model(idx, test.workspace, netcdf)
    update_ims(idx, sim.get_solution_package(f"{ex_ext[idx]}.ims"))
    return sim, None


def check_output(idx, test):
    from test_par_gwf01 import check_output as check

    check(idx, test)


@pytest.mark.parallel
@pytest.mark.parametrize("idx, name", enumerate(cases))
@pytest.mark.parametrize(
    "netcdf", [0, pytest.param(1, marks=pytest.mark.netcdf)]
)
def test_mf6model(idx, name, function_tmpdir, targets, netcdf):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t, netcdf),
        check=lambda t: check_output(idx, t),
        compare=None,
        parallel=True,
        netcdf=netcdf,
        ncpus=2,
    )
    test.run()
