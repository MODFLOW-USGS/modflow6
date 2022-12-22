import argparse
import os
import shutil
import subprocess
import sys
import textwrap
from multiprocessing import Pool
from os import PathLike
from pathlib import Path
from typing import List, Tuple

import flopy
import pymake
import pytest
from modflow_devtools.build import meson_build
from modflow_devtools.misc import get_model_paths

from utils import get_project_root_path

_verify = False
_project_root_path = get_project_root_path()
_examples_repo_path = _project_root_path.parent / "modflow6-examples"
_build_path = _project_root_path / "builddir"
_bin_path = _project_root_path / "bin"
_github_repo = "MODFLOW-USGS/modflow6"
_markdown_file_name = "run-time-comparison.md"
_is_windows = sys.platform.lower() == "win32"
_app_ext = ".exe" if _is_windows else ""
_soext = ".dll" if _is_windows else ".so"


def download_previous_version(output_path: PathLike) -> Tuple[str, Path]:
    output_path = Path(output_path).expanduser().absolute()
    version = pymake.repo_latest_version(github_repo=_github_repo, verify=_verify)
    url = (
        f"https://github.com/{_github_repo}"
        + f"/releases/download/{version}/mf{version}.zip"
    )
    pymake.download_and_unzip(
        url,
        pth=str(output_path),
        verbose=True,
        verify=_verify,
    )

    return version, output_path / f"mf{version}"


def get_mf6_cmdargs(app, argv, text="mf6:", verbose=False):
    return_text = None
    proc = subprocess.Popen(
        argv,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=os.path.dirname(app),
    )
    result, error = proc.communicate()
    if result is not None:
        c = result.decode("utf-8")
        c = c.rstrip("\r\n")
        if verbose:
            print(f"{c}")
        if text in c:
            idx0 = c.index(text) + len(text) + 1
            return_text = c[idx0:].strip()
    return return_text


def get_mf6_version(app, verbose=False):
    text = "mf6:"
    if str(app).endswith(".exe"):
        text = "mf6.exe:"
    version = get_mf6_cmdargs(app, [app, "-v"], text=text, verbose=verbose)
    if version is not None:
        version = version.split()[0]
        if verbose:
            print(f"version: {version}")
    return version


def get_mf6_compiler(app, verbose=False):
    text = "mf6:"
    if str(app).endswith(".exe"):
        text = "mf6.exe:"
    compiler = get_mf6_cmdargs(app, [app, "-c"], text=text, verbose=verbose)
    if verbose and compiler is not None:
        print(f"compiler: {compiler}")
    return compiler


def revert_files(app, example):
    replace_dict = {
        ".ims": {
            (6, 1, 1): ("dvclose", "hclose"),
        }
    }
    extensions = list(replace_dict.keys())

    # get current version
    version = get_mf6_version(app)
    if version is not None:
        version = tuple([int(v) for v in version.split(".")])

    # get a list of files in example directory
    files = os.listdir(example)

    for file in files:
        _, extension = os.path.splitext(file)
        if extension in extensions:
            key = extension.lower()
            for v, replace in replace_dict[key].items():
                if version < v:
                    fpth = os.path.join(example, file)
                    with open(fpth, "r") as f:
                        lines = f.readlines()
                    with open(fpth, "w") as f:
                        for line in lines:
                            if replace[0] in line.lower():
                                line = line.lower().replace(replace[0], replace[1])
                            f.write(line)


def get_elapsed_time(buff, tag="Elapsed run time:"):
    elt_str = ""
    for line in buff:
        if tag in line:
            i0 = line.index(":")
            elt_str = line[i0 + 1 :].strip()
    return elt_str


def time_factor(time_unit):
    if "hours" in time_unit:
        factor = 60.0 * 60.0
    elif "minutes" in time_unit:
        factor = 60.0
    else:
        factor = 1.0
    return factor


def elapsed_string_to_real(elt_str):
    time_sec = 0.0
    t = elt_str.split()
    for idx in range(0, len(t), 2):
        t0 = float(t[idx])
        time_sec += t0 * time_factor(t[idx + 1].lower())
    return time_sec


def elapsed_real_to_string(elt):
    if elt > 60.0:
        time_min = int(elt / 60.0)
        time_sec = elt % 60.0
        elt_str = f"{time_min} Minutes, "
    else:
        time_sec = elt
        elt_str = ""
    return elt_str + f"{time_sec:.3f} Seconds"


def run_function(app, example):
    return flopy.run_model(
        app,
        None,
        model_ws=example,
        silent=True,
        report=True,
    )


def run_model(current_app: PathLike, previous_app: PathLike, model_path: PathLike):
    current_app = Path(current_app).expanduser().absolute()
    previous_app = Path(previous_app).expanduser().absolute()
    model_path = Path(model_path).expanduser().absolute()

    current_time = 0.0
    previous_time = 0.0

    generic_names = ["mf6gwf", "mf6gwt"]
    name = f"{model_path.parent.name}/{model_path.name}" if model_path.name in generic_names else model_path.name
    print(f"Running scenario: {name}")
    line = f"| {name} |"

    # copy directory for previous application
    prev_dir = os.path.join(model_path, "previous")
    if os.path.isdir(prev_dir):
        shutil.rmtree(prev_dir)
    print(f"Copying {model_path} ==> {prev_dir}")
    shutil.copytree(model_path, prev_dir)

    # modify input files to use deprecated keywords in directory
    # used with the previous application
    revert_files(previous_app, prev_dir)

    # # run the current application
    # success, buff = run_function(app, example)
    #
    # # run the previous application
    # success0, buff0 = run_function(app0, prev_dir)

    # processing options
    args = (
        (current_app, model_path),
        (previous_app, prev_dir),
    )

    # Multi-processing using Pool
    # initialize the pool
    pool = Pool(processes=2)

    # run the models
    results = [pool.apply_async(run_function, args=arg) for arg in args]

    # close the pool
    pool.close()

    # set variables for processing
    success, buff = results[0].get()
    success0, buff0 = results[1].get()

    if success:
        elt = get_elapsed_time(buff)
        line += f" {elt} |"
    else:
        print(f"Failure for current app with example: {name}")
        for b in buff:
            print(b)
        line += " -- |"

    if success0:
        elt0 = get_elapsed_time(buff0)
        line += f" {elt0} |"
    else:
        print(f"Failure for previous app with example: {name}")
        line += " -- |"

    if success and success0:
        t = elapsed_string_to_real(elt)
        t0 = elapsed_string_to_real(elt0)
        current_time += t
        previous_time += t0
        pd = (t - t0) / t0
        line += f" {pd:.2%} |"
    else:
        line += " -- |"

    # clean up previous directory
    if os.path.isdir(prev_dir):
        shutil.rmtree(prev_dir)

    return success, current_time, previous_time, line


def write_results(
    current_exe: PathLike,
    previous_exe: PathLike,
    output_path: PathLike,
    current_total,
    previous_total,
    lines: List[str],
):
    current_exe = Path(current_exe)
    previous_exe = Path(previous_exe)
    output_path = Path(output_path).expanduser().absolute()

    current_v = get_mf6_version(current_exe)
    previous_v = get_mf6_version(previous_exe)

    # open markdown table
    with open(output_path / _markdown_file_name, "w") as f:
        # get version numbers and write header

        line = "### Comparison of simulation run times\n\n"
        line += (
            "Comparison of run times of the current version of "
            + f"MODFLOW 6 ({current_v}) "
            + f"to the previous version ({previous_v}). "
            + "The current example models available from the "
            + "[MODFLOW 6 Examples GitHub Repository]"
            + "(https://github.com/MODFLOW-USGS/modflow6-examples) are "
            + "used to compare run times. Simulations that fail are "
            + "indicated by '--'. The percent difference, where calculated, "
            + "is relative to the simulation run time for the previous "
            + "version. Percent differences for example problems with "
            + "short run times (less than 30 seconds) may not be significant.\n\n"
            + f"{get_mf6_compiler(current_exe, verbose=True)}.\n\n\n"
        )
        line += "| Example Problem "
        line += f"| Current Version {current_v} "
        line += f"| Previous Version {previous_v} "
        line += "| Percent difference |\n"
        line += "| :---------- | :----------: | :----------: | :----------: |\n"
        f.write(line)

        # write benchmark data
        for line in lines:
            f.write(f"{line}\n")
            f.flush()

        # add final (total) line
        pd = (current_total - previous_total) / previous_total
        line = f"| Total simulation time |"
        line += f" {elapsed_real_to_string(current_total)} |"
        line += f" {elapsed_real_to_string(previous_total)} |"
        line += f" {pd:.2%} |"
        f.write(f"{line}\n")


def run_benchmarks(
        build_path: PathLike,
        current_bin_path: PathLike,
        previous_bin_path: PathLike,
        examples_path: PathLike,
        output_path: PathLike,
        excluded: List[str]=[]):
    """Benchmark current development version against previous release with example models."""

    build_path = Path(build_path).expanduser().absolute()
    current_bin_path = Path(current_bin_path).expanduser().absolute()
    previous_bin_path = Path(previous_bin_path).expanduser().absolute()
    examples_path = Path(examples_path).expanduser().absolute()
    output_path = Path(output_path).expanduser().absolute()

    example_dirs = get_model_paths(examples_path, excluded=excluded)
    assert any(example_dirs), f"No example model paths found, have models been built?"

    # results_path = output_path / _markdown_file_name
    # if results_path.is_file():
    #     print(f"Benchmark results already exist: {results_path}")
    #     return

    exe_name = f"mf6{_app_ext}"
    current_exe = current_bin_path / exe_name
    previous_exe = previous_bin_path / exe_name

    if not current_exe.is_file():
        print(f"Building current MODFLOW 6 development version")
        meson_build(project_path=_project_root_path, build_path=build_path, bin_path=current_bin_path)

    if not previous_exe.is_file():
        version, download_path = download_previous_version(output_path)
        print(f"Rebuilding latest MODFLOW 6 release {version} in development mode")
        meson_build(project_path=download_path, build_path=build_path, bin_path=previous_bin_path)

    print(f"Benchmarking MODFLOW 6 versions:")
    print(f"    current: {current_exe}")
    print(f"    previous: {previous_exe}")

    # benchmark models
    current_total = 0.0
    previous_total = 0.0
    lines = []
    for idx, example in enumerate(example_dirs):
        success, t, t0, line = run_model(
            current_exe,
            previous_exe,
            example,
        )
        assert success, f"{example} run failed"
        current_total += t
        previous_total += t0
        lines.append(line)

    # create markdown results file
    write_results(
        current_exe=current_exe,
        previous_exe=previous_exe,
        output_path=output_path,
        current_total=current_total,
        previous_total=previous_total,
        lines=lines,
    )


@pytest.mark.skip(reason="for manual testing")
def test_run_benchmarks(tmp_path):
    run_benchmarks(
        build_path=_build_path,
        current_bin_path=_bin_path,
        previous_bin_path=_bin_path / "rebuilt",
        examples_path=_examples_repo_path / "examples",
        output_path=tmp_path,
        excluded=["previous"])
    assert (tmp_path / _markdown_file_name).is_file()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Benchmark MODFLOW 6 versions on example models",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Benchmarks the current version of MODFLOW 6 against the latest official release.
            with the example models stored in the MODFLOW-USGS/modflow6-examples repository.
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
        "--current-bin-path",
        required=False,
        default=str(_bin_path),
        help="Path to the directory to install current version executables",
    )
    parser.add_argument(
        "--previous-bin-path",
        required=False,
        default=str(_bin_path / "rebuilt"),
        help="Path to the directory to install previous version executables",
    )
    parser.add_argument(
        "-o",
        "--output-path",
        required=False,
        default=str(_project_root_path / "distribution" / ""),
        help="Location to create the zip archive",
    )
    parser.add_argument(
        "-e",
        "--examples-repo-path",
        required=False,
        default=str(_project_root_path.parent / "modflow6-examples"),
        help="Path to the directory with modflow6 examples",
    )
    args = parser.parse_args()
    build_path = Path(args.build_path)
    current_bin_path = Path(args.current_bin_path)
    previous_bin_path = Path(args.previous_bin_path)
    output_path = Path(args.output_path) if args.output_path else Path(os.getcwd())
    examples_repo_path = (
        Path(args.examples_repo_path)
        if args.examples_repo_path
        else _examples_repo_path
    )

    output_path.mkdir(parents=True, exist_ok=True)
    assert (
        examples_repo_path.is_dir()
    ), f"Examples repo not found: {examples_repo_path}"

    run_benchmarks(
        build_path=build_path,
        current_bin_path=current_bin_path,
        previous_bin_path=previous_bin_path,
        examples_path=examples_repo_path / "examples",
        output_path=output_path,
        excluded=["previous"]
    )
