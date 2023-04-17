from modflow_devtools.executables import Executables
from pytest_cases import parametrize_with_cases
from simulation import TestSimulation
from test_gwf_maw04 import GwfMaw04Cases
from test_gwf_maw_cases import GwfMawCases


@parametrize_with_cases("case", cases=[GwfMawCases, GwfMaw04Cases])
def test_gwf_models(case, targets: Executables):
    data, sim, cmp, exfunc = case
    sim.write_simulation()
    if cmp:
        cmp.write_simulation()

    test = TestSimulation(
        name=data.name,
        exe_dict=targets,
        exfunc=exfunc,
        idxsim=0,  # TODO: remove parameter from TestSimulation
        mf6_regression=True,
        require_failure=data.xfail,
        make_comparison=data.compare,
    )

    test.set_model(sim.simulation_data.mfpath.get_sim_path(), testModel=False)
    test.run()
    test.compare()
