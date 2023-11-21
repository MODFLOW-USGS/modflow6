import platform
import sys
from os import PathLike
from pathlib import Path
from typing import Optional
from warnings import warn

import pytest
from modflow_devtools.executables import Executables, get_suffixes
from modflow_devtools.misc import run_cmd

pytest_plugins = ["modflow_devtools.fixtures"]
project_root_path = Path(__file__).resolve().parent.parent


def get_version(path: PathLike = None, flag: str = "-v") -> Optional[str]:
    out, err, ret = run_cmd(str(path), flag)
    if ret == 0:
        out = "".join(out).strip()
        return out.split(":")[1].strip().split(" ")[0]
    else:
        raise ValueError(f"Failed to parse version from:\n{out + err}")


def should_compare(
    test: str, comparisons: dict, executables: Executables
) -> bool:
    if test in comparisons.keys():
        dev_ver = get_version(path=executables.mf6)
        reg_ver = get_version(path=executables.mf6_regression)
        print(f"MODFLOW 6 development version: {dev_ver}")
        print(f"MODFLOW 6 regression version: {reg_ver}")
        excluded = list(comparisons[test])
        if reg_ver in excluded:
            print(
                f"Regression version {reg_ver} not supported for test {test}, skipping comparison"
            )
            return False
    return True


_lib_exts = {
    "Darwin": ".dylib",
    "Linux": ".so",
    "Windows": ".dll",
}
_exe_ext, _lib_ext = get_suffixes(sys.platform)
_binaries_path = project_root_path / "bin"
_dl_bin_path = _binaries_path / "downloaded"
_rb_bin_path = _binaries_path / "rebuilt"
_binaries = {
    "development": [
        ("mf6", _binaries_path / f"mf6{_exe_ext}"),
        ("libmf6", _binaries_path / f"libmf6{_lib_ext}"),
        ("mf5to6", _binaries_path / f"mf5to6{_exe_ext}"),
        ("zbud6", _binaries_path / f"zbud6{_exe_ext}"),
    ],
    "downloaded": [
        ("mf2000", _dl_bin_path / f"mf2000{_exe_ext}"),
        ("mf2005", _dl_bin_path / f"mf2005dbl{_exe_ext}"),
        ("mfnwt", _dl_bin_path / f"mfnwtdbl{_exe_ext}"),
        ("mfusg", _dl_bin_path / f"mfusgdbl{_exe_ext}"),
        ("mflgr", _dl_bin_path / f"mflgrdbl{_exe_ext}"),
        ("mf2005s", _dl_bin_path / f"mf2005{_exe_ext}"),
        ("mt3dms", _dl_bin_path / f"mt3dms{_exe_ext}"),
        ("crt", _dl_bin_path / f"crt{_exe_ext}"),
        ("gridgen", _dl_bin_path / f"gridgen{_exe_ext}"),
        ("mp6", _dl_bin_path / f"mp6{_exe_ext}"),
        ("mp7", _dl_bin_path / f"mp7{_exe_ext}"),
        ("swtv4", _dl_bin_path / f"swtv4{_exe_ext}"),
        ("sutra", _dl_bin_path / f"sutra{_exe_ext}"),
        ("triangle", _dl_bin_path / f"triangle{_exe_ext}"),
        ("vs2dt", _dl_bin_path / f"vs2dt{_exe_ext}"),
        ("zonbudusg", _dl_bin_path / f"zonbudusg{_exe_ext}"),
    ],
    "rebuilt": [
        ("mf6_regression", _rb_bin_path / f"mf6{_exe_ext}"),
        ("libmf6_regression", _rb_bin_path / f"libmf6{_lib_ext}"),
        ("mf5to6_regression", _rb_bin_path / f"mf5to6{_exe_ext}"),
        ("zbud6_regression", _rb_bin_path / f"zbud6{_exe_ext}"),
    ],
}


@pytest.fixture(scope="session")
def bin_path() -> Path:
    return _binaries_path


@pytest.fixture(scope="session")
def libmf6_path() -> Path:
    return _binaries_path / f"libmf6{_lib_exts[platform.system()]}"


@pytest.fixture(scope="session")
def targets() -> Executables:
    d = dict()

    # require development binaries
    for k, v in _binaries["development"]:
        assert v.is_file(), f"Couldn't find binary '{k}' expected at: {v}"
        d[k] = v

    # downloaded/rebuilt binaries are optional
    for k, v in _binaries["downloaded"] + _binaries["rebuilt"]:
        if v.is_file():
            d[k] = v
        else:
            warn(f"Couldn't find binary '{k}' expected at: {v}")

    return Executables(**d)


def try_get_target(targets: Executables, name: str) -> Path:
    """Try to retrieve the path to a binary. If the binary is a development
    target and can't be found, an error is raised. Otherwise (if the binary
    is downloaded or rebuilt) the test is skipped. This is to allow testing
    without downloaded or rebuilt binaries, e.g. if the network is down."""

    try:
        # modflow-devtools >= 1.3
        exe = targets.get(name)
        if exe:
            return exe
        elif name in _binaries["development"]:
            raise ValueError(f"Couldn't find binary '{name}'")
        else:
            pytest.skip(f"Couldn't find binary '{name}'")
    except AttributeError:
        # modflow-devtools < 1.3
        try:
            return targets[name]
        except:
            if name in _binaries["development"]:
                raise ValueError(f"Couldn't find binary '{name}'")
            else:
                pytest.skip(f"Couldn't find binary 'gridgen'")


@pytest.fixture
def original_regression(request) -> bool:
    return request.config.getoption("--original-regression")


@pytest.fixture(scope="session")
def markers(pytestconfig) -> str:
    return pytestconfig.getoption("-m")


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
        help="include parallel test cases",
    )


def pytest_collection_modifyitems(config, items):
    if config.getoption("--parallel"):
        # --parallel given in cli: do not skip parallel tests
        return
    skip_parallel = pytest.mark.skip(reason="need --parallel option to run")
    for item in items:
        if "parallel" in item.keywords:
            item.add_marker(skip_parallel)
