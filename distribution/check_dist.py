import platform
import subprocess
from os import environ
from pathlib import Path

import pytest

_system = platform.system()
_eext = ".exe" if _system == "Windows" else ""
_soext = ".dll" if _system == "Windows" else ".so" if _system == "Linux" else ".dylib"
_scext = ".bat" if _system == "Windows" else ".sh"
_fc = environ.get("FC", None)


@pytest.fixture
def dist_dir_path(request):
    def skip():
        pytest.skip(f"no distribution directory found at {path}")

    path = request.config.getoption("--path")
    if not path:
        skip()

    path = Path(path).expanduser().absolute()
    if not path.is_dir():
        skip()

    return path


def test_sources(dist_dir_path):
    assert (dist_dir_path / "src").is_dir()
    assert (dist_dir_path / "src" / "mf6.f90").is_file()


@pytest.mark.skipif(not _fc, reason="needs Fortran compiler")
def test_makefiles(dist_dir_path):
    assert (dist_dir_path / "make" / "makefile").is_file()
    assert (dist_dir_path / "make" / "makedefaults").is_file()
    assert (dist_dir_path / "utils" / "zonebudget" / "make" / "makefile").is_file()
    assert (dist_dir_path / "utils" / "zonebudget" / "make" / "makedefaults").is_file()
    assert (dist_dir_path / "utils" / "mf5to6" / "make" / "makefile").is_file()
    assert (dist_dir_path / "utils" / "mf5to6" / "make" / "makedefaults").is_file()

    # makefiles can't be used on Windows with ifort compiler
    if _system != 'Windows' or _fc != 'ifort':
        print(subprocess.check_output("make", cwd=dist_dir_path / "make", shell=True))
        print(subprocess.check_output("make", cwd=dist_dir_path / "utils" / "zonebudget" / "make", shell=True))
        print(subprocess.check_output("make", cwd=dist_dir_path / "utils" / "mf5to6" / "make", shell=True))


def test_msvs(dist_dir_path):
    assert (dist_dir_path / "msvs" / "mf6.sln").is_file()
    assert (dist_dir_path / "msvs" / "mf6.vfproj").is_file()
    assert (dist_dir_path / "msvs" / "mf6bmi.sln").is_file()
    assert (dist_dir_path / "msvs" / "mf6bmi.vfproj").is_file()
    assert (dist_dir_path / "msvs" / "mf6core.vfproj").is_file()


def test_docs(dist_dir_path):
    assert (dist_dir_path / "doc" / "mf6io.pdf").is_file()
    assert (dist_dir_path / "doc" / "release.pdf").is_file()
    assert (dist_dir_path / "doc" / "mf5to6.pdf").is_file()
    assert (dist_dir_path / "doc" / "zonebudget.pdf").is_file()
    assert (dist_dir_path / "doc" / "mf6suptechinfo.pdf").is_file()
    assert (dist_dir_path / "doc" / "mf6examples.pdf").is_file()

    for pub in [
        "tm6a55",
        "tm6a56",
        "tm6a57",
        "tm6a61",
        "tm6a62",
    ]:
        assert (dist_dir_path / "doc" / f"{pub}.pdf").is_file()


def test_examples(dist_dir_path):
    examples_path = dist_dir_path / "examples"
    example_path = next(examples_path.iterdir(), None)
    assert example_path
    output = ' '.join(subprocess.check_output([str(example_path / f"run{_scext}")], cwd=example_path).decode().split())
    print(output)


def test_binaries(dist_dir_path):
    bin_path = dist_dir_path / "bin"
    assert (bin_path / f"mf6{_eext}").is_file()
    assert (bin_path / f"zbud6{_eext}").is_file()
    assert (bin_path / f"mf5to6{_eext}").is_file()
    assert (bin_path / f"libmf6{_soext}").is_file()

    output = ' '.join(subprocess.check_output([str(bin_path / f"mf6{_eext}"), "-v"]).decode().split())
    assert output.startswith("mf6")
    assert output.lower().count("release") == 1
    assert output.lower().count("candidate") <= 1

    version = output.lower().rpartition(":")[2].rpartition("release")[0].strip()
    v_split = version.split(".")
    assert len(v_split) == 3
    assert all(s.isdigit() for s in v_split)

    # TODO check utils
