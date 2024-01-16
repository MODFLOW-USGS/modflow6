import argparse
import os
import platform
import shutil
import sys
import textwrap
from os import PathLike, environ
from pathlib import Path
from pprint import pprint
from shutil import copytree

import pytest
from modflow_devtools.build import meson_build
from modflow_devtools.download import download_and_unzip, get_release
from modflow_devtools.markers import requires_exe
from modflow_devtools.misc import get_model_paths

from build_docs import build_documentation
from build_makefiles import (build_mf5to6_makefile, build_mf6_makefile,
                             build_zbud6_makefile)
from utils import get_project_root_path, run_command

# default paths
_project_root_path = get_project_root_path()
_examples_repo_path = _project_root_path.parent / "modflow6-examples"
_build_path = _project_root_path / "builddir"

# OS-specific extensions
_system = platform.system()
_eext = ".exe" if _system == "Windows" else ""
_soext = ".dll" if _system == "Windows" else ".so" if _system == "Linux" else ".dylib"
_scext = ".bat" if _system == "Windows" else ".sh"
_executable = f"mf6{_eext}"

# Fortran and C compilers
FC = environ.get("FC", "gfortran")
CC = environ.get("CC", "gcc")


def copy_sources(output_path: PathLike):
    output_path = Path(output_path).expanduser().absolute()

    # make sure output directory exists
    output_path.mkdir(exist_ok=True)

    # Copy Visual Studio sln and project files
    print("Copying msvs files to output directory")
    (output_path / "msvs").mkdir(exist_ok=True)
    source_msvs_path = _project_root_path / "msvs"
    for d in [
        str(source_msvs_path / "mf6.sln"),
        str(source_msvs_path / "mf6.vfproj"),
        str(source_msvs_path / "mf6core.vfproj"),
        str(source_msvs_path / "mf6bmi.sln"),
        str(source_msvs_path / "mf6bmi.vfproj"),
    ]:
        shutil.copy(d, output_path / "msvs")

    ignored = shutil.ignore_patterns(".DS_Store")

    # copy top-level meson.build and meson.options
    shutil.copy(_project_root_path / "meson.build", output_path)
    shutil.copy(_project_root_path / "meson.options", output_path)

    # copy source folder
    src_path = _project_root_path / "src"
    dst_path = output_path / "src"
    print(f"Copying {src_path} to {dst_path}")
    copytree(src_path, dst_path, ignore=ignored)

    # copy srcbmi folder
    src_path = _project_root_path / "srcbmi"
    dst_path = output_path / "srcbmi"
    print(f"Copying {src_path} to {dst_path}")
    copytree(src_path, dst_path, ignore=ignored)

    # copy utils folder
    src_path = _project_root_path / "utils"
    dst_path = output_path / "utils"
    print(f"Copying {src_path} to {dst_path}")
    copytree(src_path, dst_path, ignore=ignored)


def test_copy_sources(tmp_path):
    copy_sources(tmp_path)

    assert (tmp_path / "src").is_dir()
    assert (tmp_path / "srcbmi").is_dir()
    assert (tmp_path / "utils").is_dir()
    assert (tmp_path / "msvs").is_dir()

    assert (tmp_path / "src" / "meson.build").is_file()
    assert (tmp_path / "srcbmi" / "meson.build").is_file()
    assert (tmp_path / "utils" / "meson.build").is_file()
    assert (tmp_path / "msvs" / "mf6.sln").is_file()


def build_examples(examples_repo_path: PathLike, overwrite: bool = False):
    examples_repo_path = Path(examples_repo_path).expanduser().absolute()

    # create examples, but don't run them
    examples_path = examples_repo_path / "examples"
    examples_path.mkdir(parents=True, exist_ok=True)
    if not overwrite and any(get_model_paths(examples_path)):
        print(f"Examples already built")
    else:
        print(f"Building examples")
        scripts_folder = examples_repo_path / "scripts"
        exclude_list = ["ex-gwf-capture.py"]
        scripts = [
            fname
            for fname in scripts_folder.glob("*")
            if fname.suffix == ".py"
            and fname.stem.startswith("ex-")
            and fname.stem not in exclude_list
        ]
        for script in scripts:
            argv = [
                sys.executable,
                script,
                "--no_run",
                "--no_plot",
                "--destination",
                examples_path,
            ]
            print(f"running {argv} in {scripts_folder}")
            run_command(argv, scripts_folder)


def setup_examples(
    bin_path: PathLike, examples_path: PathLike, overwrite: bool = False
):
    examples_path = Path(examples_path).expanduser().absolute()

    # download example models zip asset
    latest = get_release("MODFLOW-USGS/modflow6-examples", "latest")
    assets = latest["assets"]
    asset = next(
        iter([a for a in assets if a["name"] == "modflow6-examples.zip"]), None
    )
    download_and_unzip(asset["browser_download_url"], examples_path, verbose=True)

    # list folders with mfsim.nam (recursively)
    # and add run.sh/bat script to each folder
    model_paths = get_model_paths(examples_path)
    for mp in model_paths:
        script_path = mp / f"run{_scext}"
        if not overwrite and script_path.is_file():
            print(f"Script {script_path} already exists")
        else:
            print(f"Creating {script_path}")
            with open(script_path, "w") as f:
                if _system == "Windows":
                    f.write("@echo off" + "\n")
                else:
                    f.write("#!/bin/sh" + "\n")
                runbatloc = os.path.relpath(bin_path / _executable, start=mp)
                f.write(runbatloc + "\n")
                if _system == "Windows":
                    f.write("echo." + "\n")
                    f.write("echo Run complete.  Press any key to continue" + "\n")
                    f.write("pause>nul" + "\n")

            if _system != "Windows":
                script_path.chmod(script_path.stat().st_mode | 0o111)
                print(f"Execute permission set for {script_path}")

    # add runall.sh/bat, which runs all examples
    script_path = examples_path / f"runall{_scext}"
    if not overwrite and script_path.is_file():
        print(f"Script {script_path} already exists")
    else:
        print(f"Creating {script_path}")
        with open(script_path, "w") as f:
            if _system != "Windows":
                f.write("#!/bin/sh" + "\n")
            for mp in model_paths:
                d = os.path.relpath(mp, start=examples_path)
                s = f"cd {d}"
                f.write(s + "\n")
                runbatloc = os.path.relpath(bin_path / _executable, start=mp)
                f.write(runbatloc + "\n")
                d = os.path.relpath(examples_path, start=mp)
                s = f"cd {d}"
                f.write(s + "\n")
                s = ""
                f.write(s + "\n")
            if _system == "Windows":
                f.write("pause" + "\n")
            else:
                script_path.chmod(script_path.stat().st_mode | 0o111)
                print(f"Execute permission set for {script_path}")


def build_programs_meson(
    build_path: PathLike, bin_path: PathLike, overwrite: bool = False
):
    build_path = Path(build_path).expanduser().absolute()
    bin_path = Path(bin_path).expanduser().absolute()

    exe_paths = [
        bin_path / f"mf6{_eext}",
        bin_path / f"zbud6{_eext}",
        bin_path / f"mf5to6{_eext}",
    ]
    lib_paths = [bin_path / f"libmf6{_soext}"]

    if (
        not overwrite
        and all(p.is_file() for p in exe_paths)
        and all(p.is_file() for p in lib_paths)
    ):
        print(f"Binaries already exist:")
        pprint(exe_paths + lib_paths)
    else:
        print(f"Building binaries in {build_path}, installing to {bin_path}")
        meson_build(
            project_path=_project_root_path, build_path=build_path, bin_path=bin_path
        )

    for target in exe_paths + lib_paths:
        assert target.is_file(), f"Failed to build {target}"
        target.chmod(target.stat().st_mode | 0o111)
        print(f"Execute permission set for {target}")


def test_build_programs_meson(tmp_path):
    build_programs_meson(tmp_path / "builddir", tmp_path / "bin")


def build_makefiles(output_path: PathLike):
    output_path = Path(output_path).expanduser().absolute()

    # create and copy mf6 makefile
    build_mf6_makefile()
    (output_path / "make").mkdir(parents=True, exist_ok=True)
    shutil.copyfile(
        _project_root_path / "make" / "makefile", output_path / "make" / "makefile"
    )
    shutil.copyfile(
        _project_root_path / "make" / "makedefaults",
        output_path / "make" / "makedefaults",
    )

    # create and copy zbud6 makefile
    build_zbud6_makefile()
    rel_path = Path("utils") / "zonebudget" / "make"
    (output_path / rel_path).mkdir(parents=True, exist_ok=True)
    shutil.copyfile(
        _project_root_path / rel_path / "makefile", output_path / rel_path / "makefile"
    )
    shutil.copyfile(
        _project_root_path / rel_path / "makedefaults",
        output_path / rel_path / "makedefaults",
    )

    # create and copy mf5to6 makefile
    build_mf5to6_makefile()
    rel_path = Path("utils") / "mf5to6" / "make"
    (output_path / rel_path).mkdir(parents=True, exist_ok=True)
    shutil.copyfile(
        _project_root_path / rel_path / "makefile", output_path / rel_path / "makefile"
    )
    shutil.copyfile(
        _project_root_path / rel_path / "makedefaults",
        output_path / rel_path / "makedefaults",
    )


def test_build_makefiles(tmp_path):
    build_makefiles(tmp_path)

    assert (tmp_path / "make" / "makefile").is_file()
    assert (tmp_path / "make" / "makedefaults").is_file()
    assert (tmp_path / "utils" / "zonebudget" / "make" / "makefile").is_file()
    assert (tmp_path / "utils" / "zonebudget" / "make" / "makedefaults").is_file()
    assert (tmp_path / "utils" / "mf5to6" / "make" / "makefile").is_file()
    assert (tmp_path / "utils" / "mf5to6" / "make" / "makedefaults").is_file()

    os.system(f"cd {tmp_path} && make -f make/makefile")
    os.system(f"cd {tmp_path} && make -f utils/zonebudget/make/makefile")
    os.system(f"cd {tmp_path} && make -f utils/mf5to6/make/makefile")


def build_distribution(
    build_path: PathLike,
    output_path: PathLike,
    examples_repo_path: PathLike,
    full: bool = False,
    overwrite: bool = False,
):
    print(f"Building {'full' if full else 'minimal'} distribution")

    build_path = Path(build_path).expanduser().absolute()
    output_path = Path(output_path).expanduser().absolute()
    examples_repo_path = Path(examples_repo_path).expanduser().absolute()

    # binaries
    build_programs_meson(
        build_path=build_path, bin_path=output_path / "bin", overwrite=overwrite
    )

    # code.json metadata
    shutil.copy(_project_root_path / "code.json", output_path)

    # full releases include examples, source code, makefiles and docs
    if not full:
        return

    # examples
    setup_examples(
        bin_path=output_path / "bin",
        examples_path=output_path / "examples",
        overwrite=overwrite,
    )

    # copy source code files
    copy_sources(output_path=output_path)

    # build and copy makefiles
    build_makefiles(output_path=output_path)

    # docs
    build_documentation(
        bin_path=output_path / "bin",
        full=full,
        output_path=output_path / "doc",
        overwrite=overwrite,
    )


@requires_exe("pdflatex")
@pytest.mark.skip(reason="manual testing")
@pytest.mark.parametrize("full", [True, False])
def test_build_distribution(tmp_path, full):
    output_path = tmp_path / "dist"
    build_distribution(
        build_path=tmp_path / "builddir",
        output_path=output_path,
        examples_repo_path=_examples_repo_path,
        full=full,
        overwrite=True,
    )

    if full:
        # todo
        pass
    else:
        # check binaries and libs
        system = platform.system()
        ext = ".exe" if system == "Windows" else ""
        for exe in ["mf6", "mf5to6", "zbud6"]:
            assert (output_path / f"{exe}{ext}").is_file()
        assert (
            output_path
            / (
                "libmf6"
                + (
                    ".so"
                    if system == "Linux"
                    else (".dylib" if system == "Darwin" else ".dll")
                )
            )
        ).is_file()

        # check mf6io docs
        assert (output_path / "mf6io.pdf").is_file()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Create a Modflow 6 distribution directory for release",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Create a distribution folder. If no output path is provided
            distribution files are written to the distribution/ folder.
            By default a minimal distribution containing only binaries,
            mf6io documentation, release notes and metadata (code.json)
            is created. To create a full distribution including sources
            and examples, use the --full flag.
            """
        ),
    )
    parser.add_argument(
        "--build-path",
        required=False,
        default=str(_build_path),
        help="Path to the build workspace",
    )
    parser.add_argument(
        "-o",
        "--output-path",
        required=False,
        default=str(_project_root_path / "distribution"),
        help="Path to create distribution artifacts",
    )
    parser.add_argument(
        "-e",
        "--examples-repo-path",
        required=False,
        default=str(_examples_repo_path),
        help="Path to directory containing modflow6 example models",
    )
    parser.add_argument(
        "--full",
        required=False,
        default=False,
        action="store_true",
        help="Build a full rather than minimal distribution",
    )
    parser.add_argument(
        "-f",
        "--force",
        required=False,
        default=False,
        action="store_true",
        help="Recreate and overwrite existing artifacts",
    )
    args = parser.parse_args()
    build_path = Path(args.build_path)
    out_path = Path(args.output_path)
    examples_repo_path = (
        Path(args.examples_repo_path)
        if args.examples_repo_path
        else _examples_repo_path
    )
    assert (
        examples_repo_path.is_dir()
    ), f"Examples repo not found at path: {examples_repo_path}"
    out_path.mkdir(parents=True, exist_ok=True)

    build_distribution(
        build_path=build_path,
        output_path=out_path,
        examples_repo_path=examples_repo_path,
        full=args.full,
        overwrite=args.force,
    )
