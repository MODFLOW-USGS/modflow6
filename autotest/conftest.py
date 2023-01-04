from pathlib import Path

import pytest
from modflow_devtools.executables import Executables, build_default_exe_dict

pytest_plugins = ["modflow_devtools.fixtures"]
project_root_path = Path(__file__).parent.parent


def should_compare(test: str, comparisons: dict, executables: Executables) -> bool:
    if test in comparisons.keys():
        version = Executables.get_version(path=executables.mf6)
        print(f"MODFLOW 6 development version='{version}'")
        version = Executables.get_version(path=executables.mf6_regression)
        print(f"MODFLOW 6 regression version='{version}'")
        if version in comparisons[test]:
            print(
                f"Test {test} does not run with versions {comparisons[test]}"
            )
            print(
                f"Skipping regression test of sim {test} because the version is {version}"
            )
            return False
    return True


@pytest.fixture(scope="session")
def bin_path() -> Path:
    return project_root_path / "bin"


@pytest.fixture(scope="session")
def targets(bin_path) -> Executables:
    return Executables(**build_default_exe_dict(bin_path))


@pytest.fixture
def original_regression(request) -> bool:
    oreg = request.config.getoption("--original-regression")
    return oreg


def pytest_addoption(parser):
    parser.addoption(
        "--original-regression",
        action="store_true",
        default=False,
        help="TODO"
    )
