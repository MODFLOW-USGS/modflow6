import platform
import subprocess
from os import environ
from pathlib import Path
from pprint import pprint

import pytest


# OS-specific extensions
_system = platform.system()
_eext = ".exe" if _system == "Windows" else ""
_soext = ".dll" if _system == "Windows" else ".so" if _system == "Linux" else ".dylib"
_scext = ".bat" if _system == "Windows" else ".sh"

# fortran compiler
_fc = environ.get("FC", None)

# directories included in full distribution
_included_dir_paths = {
    "full": [
        "bin",
        "doc",
        "examples",
        "src",
        "srcbmi",
        "msvs",
        "make",
        "utils",
    ],
    "minimal": [
        "bin",
        "doc",
    ],
}


@pytest.fixture
def approved(request):
    return request.config.getoption("--approved")


@pytest.fixture
def releasemode(request):
    return request.config.getoption("--releasemode")


@pytest.fixture
def full(request):
    return request.config.getoption("--full")


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


def test_directories(dist_dir_path, full):
    for dir_path in _included_dir_paths["full" if full else "minimal"]:
        assert (dist_dir_path / dir_path).is_dir()


def test_sources(dist_dir_path, releasemode, full):
    if not full:
        pytest.skip(reason="sources not included in minimal distribution")

    assert (dist_dir_path / "src").is_dir()
    assert (dist_dir_path / "src" / "mf6.f90").is_file()

    version_file_path = dist_dir_path / "src" / "Utilities" / "version.f90"
    assert version_file_path.is_file()

    # find IDEVELOPMODE line
    lines = open(version_file_path, "r").read().splitlines()
    pattern = ":: IDEVELOPMODE ="
    line = next(iter([l for l in lines if pattern in l]), None)
    assert line

    # make sure IDEVELOPMODE was set correctly
    idevelopmode = 0 if releasemode else 1
    assert f"IDEVELOPMODE = {idevelopmode}" in line


@pytest.mark.skipif(not _fc, reason="needs Fortran compiler")
def test_makefiles(dist_dir_path, full):
    if not full:
        pytest.skip(reason="makefiles not included in minimal distribution")

    assert (dist_dir_path / "make" / "makefile").is_file()
    assert (dist_dir_path / "make" / "makedefaults").is_file()
    assert (dist_dir_path / "utils" / "zonebudget" / "make" / "makefile").is_file()
    assert (dist_dir_path / "utils" / "zonebudget" / "make" / "makedefaults").is_file()
    assert (dist_dir_path / "utils" / "mf5to6" / "make" / "makefile").is_file()
    assert (dist_dir_path / "utils" / "mf5to6" / "make" / "makedefaults").is_file()

    # makefiles can't be used on Windows with ifort compiler
    if _system != "Windows" or _fc != "ifort":
        print(subprocess.check_output("make", cwd=dist_dir_path / "make", shell=True))
        print(
            subprocess.check_output(
                "make", cwd=dist_dir_path / "utils" / "zonebudget" / "make", shell=True
            )
        )
        print(
            subprocess.check_output(
                "make", cwd=dist_dir_path / "utils" / "mf5to6" / "make", shell=True
            )
        )


def test_msvs(dist_dir_path, full):
    if not full:
        pytest.skip(reason="MSVS files not included in minimal distribution")

    assert (dist_dir_path / "msvs" / "mf6.sln").is_file()
    assert (dist_dir_path / "msvs" / "mf6.vfproj").is_file()
    assert (dist_dir_path / "msvs" / "mf6bmi.sln").is_file()
    assert (dist_dir_path / "msvs" / "mf6bmi.vfproj").is_file()
    assert (dist_dir_path / "msvs" / "mf6core.vfproj").is_file()


def test_docs(dist_dir_path, full):
    # mf6io should always be included
    assert (dist_dir_path / "doc" / "mf6io.pdf").is_file()

    if full:
        # check other custom-built documentation
        assert (dist_dir_path / "doc" / "release.pdf").is_file()
        assert (dist_dir_path / "doc" / "mf5to6.pdf").is_file()
        assert (dist_dir_path / "doc" / "zonebudget.pdf").is_file()
        assert (dist_dir_path / "doc" / "mf6suptechinfo.pdf").is_file()
        assert (dist_dir_path / "doc" / "mf6examples.pdf").is_file()

        # check publications downloaded from USGS site
        for pub in [
            "tm6a55",
            "tm6a56",
            "tm6a57",
            "tm6a61",
            "tm6a62",
        ]:
            assert (dist_dir_path / "doc" / f"{pub}.pdf").is_file()


def test_examples(dist_dir_path, full):
    if not full:
        pytest.skip(reason="examples not included in minimal distribution")

    examples_path = dist_dir_path / "examples"
    assert examples_path.is_dir()
    assert (examples_path / f"runall{_scext}").is_file()
    example_paths = [
        p for p in examples_path.glob("*") if p.is_dir() and p.stem.startswith("ex")
    ]
    print(f"{len(example_paths)} example models found:")
    pprint(example_paths)
    for p in example_paths:
        script_path = p / f"run{_scext}"
        if not script_path.is_file():
            continue
        pprint(subprocess.check_output([str(script_path)], cwd=p).decode().split())
        break


def test_binaries(dist_dir_path, approved):
    bin_path = dist_dir_path / "bin"
    assert (bin_path / f"mf6{_eext}").is_file()
    assert (bin_path / f"zbud6{_eext}").is_file()
    assert (bin_path / f"mf5to6{_eext}").is_file()
    assert (bin_path / f"libmf6{_soext}").is_file()

    # get version string
    output = " ".join(
        subprocess.check_output([str(bin_path / f"mf6{_eext}"), "-v"]).decode().split()
    ).lower()
    assert output.startswith("mf6")

    # make sure version string reflects approval
    assert ("preliminary" in output) != approved

    # check version numbers
    version = output.lower().split(" ")[1]
    print(version)
    v_split = version.split(".")
    assert len(v_split) >= 3
    assert all(s.isdigit() for s in v_split[:3])
    if approved:
        assert len(v_split) == 3
    else:
        assert "dev" in v_split[3]
