import platform
from pathlib import Path

import pytest
from modflow_devtools.executables import Executables, build_default_exe_dict

pytest_plugins = ["modflow_devtools.fixtures"]
project_root_path = Path(__file__).resolve().parent.parent


def should_compare(
    test: str, comparisons: dict, executables: Executables
) -> bool:
    if test in comparisons.keys():
        dev_ver = Executables.get_version(path=executables.mf6).split(" ")[0]
        reg_ver = Executables.get_version(
            path=executables.mf6_regression
        ).split(" ")[0]
        print(f"MODFLOW 6 development version: {dev_ver}")
        print(f"MODFLOW 6 regression version: {reg_ver}")
        excluded = list(comparisons[test])
        if reg_ver in excluded:
            print(
                f"Regression version {reg_ver} not supported for test {test}, skipping comparison"
            )
            return False
    return True


@pytest.fixture(scope="session")
def bin_path() -> Path:
    return project_root_path / "bin"


@pytest.fixture(scope="session")
def libmf6_path(bin_path) -> Path:
    ext = {
        "Darwin": ".dylib",
        "Linux": ".so",
        "Windows": ".dll",
    }[platform.system()]
    lib_name = bin_path / f"libmf6{ext}"
    return lib_name


@pytest.fixture(scope="session")
def targets(bin_path) -> Executables:
    return Executables(**build_default_exe_dict(bin_path))


@pytest.fixture
def original_regression(request) -> bool:
    return request.config.getoption("--original-regression")


@pytest.fixture(scope="session")
def markers(pytestconfig) -> str:
    return pytestconfig.getoption('-m')


def pytest_addoption(parser):
    parser.addoption(
        "--original-regression",
        action="store_true",
        default=False,
        help="TODO",
    )
    parser.addoption(
        "--parallel", 
        action="store_true", 
        default=False, 
        help="include parallel test cases"
    )


def pytest_collection_modifyitems(config, items):
    if config.getoption("--parallel"):
        # --parallel given in cli: do not skip parallel tests
        return
    skip_parallel = pytest.mark.skip(reason="need --parallel option to run")
    for item in items:
        if "parallel" in item.keywords:
            item.add_marker(skip_parallel)
