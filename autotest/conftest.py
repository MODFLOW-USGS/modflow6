import platform
import sys
from pathlib import Path
from os import PathLike
from typing import Dict, Optional

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
    exe_ext, lib_ext = get_suffixes(sys.platform)
    dl_bin = bin_path / "downloaded"
    rb_bin = bin_path / "rebuilt"
    tgts = dict()

    # downloaded executables
    tgts["mf2000"] = dl_bin / f"mf2000{exe_ext}"
    tgts["mf2005"] = dl_bin / f"mf2005dbl{exe_ext}"
    tgts["mfnwt"] = dl_bin / f"mfnwtdbl{exe_ext}"
    tgts["mfusg"] = dl_bin / f"mfusgdbl{exe_ext}"
    tgts["mflgr"] = dl_bin / f"mflgrdbl{exe_ext}"
    tgts["mf2005s"] = dl_bin / f"mf2005{exe_ext}"
    tgts["mt3dms"] = dl_bin / f"mt3dms{exe_ext}"
    tgts["crt"] = dl_bin / f"crt{exe_ext}"
    tgts["gridgen"] = dl_bin / f"gridgen{exe_ext}"
    tgts["mp6"] = dl_bin / f"mp6{exe_ext}"
    tgts["mp7"] = dl_bin / f"mp7{exe_ext}"
    tgts["swtv4"] = dl_bin / f"swtv4{exe_ext}"
    tgts["sutra"] = dl_bin / f"sutra{exe_ext}"
    tgts["triangle"] = dl_bin / f"triangle{exe_ext}"
    tgts["vs2dt"] = dl_bin / f"vs2dt{exe_ext}"
    tgts["zonbudusg"] = dl_bin / f"zonbudusg{exe_ext}"

    # binaries rebuilt from last release
    tgts["mf6_regression"] = rb_bin / f"mf6{exe_ext}"
    tgts["libmf6_regression"] = rb_bin / f"libmf6{lib_ext}"
    tgts["mf5to6_regression"] = rb_bin / f"mf5to6{exe_ext}"
    tgts["zbud6_regression"] = rb_bin / f"zbud6{exe_ext}"

    # local development binaries
    tgts["mf6"] = bin_path / f"mf6{exe_ext}"
    tgts["libmf6"] = bin_path / f"libmf6{lib_ext}"
    tgts["mf5to6"] = bin_path / f"mf5to6{exe_ext}"
    tgts["zbud6"] = bin_path / f"zbud6{exe_ext}"

    return Executables(**tgts)


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
