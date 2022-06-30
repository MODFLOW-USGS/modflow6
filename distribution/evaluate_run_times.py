import os
import shutil
import subprocess
import sys
from multiprocessing import Pool

import flopy
import pymake

# Set VERIFY
VERIFY = False

# add path to build script in autotest directory and reuse mf6 build scripts
sys.path.append(os.path.join("..", "autotest"))
from build_exes import meson_build

github_repo = "MODFLOW-USGS/modflow6"
working_dir = "./temp/"
base_build_dir = os.path.join("..", "bin")
examples_dir = "examples"
app_ext = ""
if sys.platform == "win32":
    app_ext = ".exe"


def _get_version():
    version = None
    for idx, arg in enumerate(sys.argv):
        if arg == "--version":
            version = sys.argv[idx + 1]
            break
    if version is None:
        version = pymake.repo_latest_version(
            github_repo=github_repo, verify=VERIFY
        )
    return version


def _del_version():
    i0 = None
    for idx, arg in enumerate(sys.argv):
        if arg == "--version":
            i0 = idx
            break
    if i0 is not None:
        del sys.argv[i0 : i0 + 2]


def _is_dryrun():
    dryrun = False
    for idx, arg in enumerate(sys.argv):
        if arg == "--dryrun":
            dryrun = True
            break
    return dryrun


def _get_download_dir():
    return f"mf{_get_version()}"


def _get_previous_version():
    version = _get_version()
    url = (
        f"https://github.com/{github_repo}"
        + f"/releases/download/{version}/mf{version}.zip"
    )
    if not _is_dryrun():
        pymake.download_and_unzip(
            url,
            pth=working_dir,
            verbose=True,
            verify=VERIFY,
        )

    return version, f"mf{version}"


def build_previous_version(pth):
    _del_version()
    appdir = os.path.abspath(os.path.join(base_build_dir, "rebuilt"))
    if not _is_dryrun():
        meson_build(dir_path=pth, libdir=appdir)

    return os.path.abspath(os.path.join(appdir, f"mf6{app_ext}"))


def build_current_version():
    if not _is_dryrun():
        meson_build()
    return os.path.abspath(os.path.join(base_build_dir, f"mf6{app_ext}"))


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
    if app.endswith(".exe"):
        text = "mf6.exe:"
    version = get_mf6_cmdargs(app, [app, "-v"], text=text, verbose=verbose)
    if version is not None:
        version = version.split()[0]
        if verbose:
            print(f"version: {version}")
    return version


def get_mf6_compiler(app, verbose=False):
    text = "mf6:"
    if app.endswith(".exe"):
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
                                line = line.lower().replace(
                                    replace[0], replace[1]
                                )
                            f.write(line)
    return


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


def get_examples():
    examples_repo = "MODFLOW-USGS/modflow6-examples"
    version = pymake.repo_latest_version(
        github_repo=examples_repo, verify=VERIFY
    )
    print(f"current examples version: {version}")
    url = (
        f"https://github.com/{examples_repo}"
        + f"/releases/download/{version}/modflow6-examples.zip"
    )
    pth = os.path.join(working_dir, examples_dir)
    if not _is_dryrun():
        pymake.download_and_unzip(url, pth=pth, verbose=True, verify=VERIFY)
    example_files = []
    for root, dirs, files in os.walk(pth):
        fpth = os.path.join(root, "mfsim.nam")
        if os.path.exists(fpth):
            example_files.append(os.path.abspath(root))
    return sorted(example_files)


def run_function(app, example):
    return flopy.run_model(
        app,
        None,
        model_ws=example,
        silent=True,
        report=True,
    )


def run_model(app, app0, example, fmd, silent=True, pool=False):
    t_out = 0.0
    t0_out = 0.0

    id0 = example.index(examples_dir) + len(examples_dir) + 1
    test = example[id0:]
    print(f"Running simulation: {test}")
    line = f"| {test} |"

    # copy directory for previous application
    prev_dir = os.path.join(example, "previous")
    if os.path.isdir(prev_dir):
        shutil.rmtree(prev_dir)
    print(f"Copying {example} ==> {prev_dir}")
    shutil.copytree(example, prev_dir)

    # modify input files to use deprecated keywords in directory
    # used with the previous application
    revert_files(app0, prev_dir)

    # # run the current application
    # success, buff = run_function(app, example)
    #
    # # run the previous application
    # success0, buff0 = run_function(app0, prev_dir)

    # processing options
    args = (
        (app, example),
        (app0, prev_dir),
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
        line += " -- |"

    if success0:
        elt0 = get_elapsed_time(buff0)
        line += f" {elt0} |"
    else:
        line += " -- |"

    if success and success0:
        t = elapsed_string_to_real(elt)
        t0 = elapsed_string_to_real(elt0)
        t_out += t
        t0_out += t0
        pd = (t - t0) / t0
        line += f" {pd:.2%} |"
    else:
        line += " -- |"

    fmd.write(f"{line}\n")
    fmd.flush()

    # clean up previous directory
    if os.path.isdir(prev_dir):
        shutil.rmtree(prev_dir)

    return success, t_out, t0_out


def cleanup():
    b = None
    if not _is_dryrun():
        b = True
    return


if __name__ == "__main__":
    _get_previous_version()

    # compile the previous version
    pth = os.path.join(working_dir, _get_download_dir())
    previous_app = build_previous_version(pth)

    # compile the current version
    current_app = build_current_version()
    print(f"previous app: {previous_app}\ncurrent app: {current_app}")

    # open markdown table
    f = open("run-time-comparison.md", "w")

    # get version numbers and write header
    v = get_mf6_version(current_app)
    v0 = get_mf6_version(previous_app)
    line = "### Comparison of simulation run times\n\n"
    line += (
        "Comparison of run times of the current version of "
        + f"MODFLOW 6 ({v}) "
        + f"to the previous version ({v0}). "
        + "The current example models available from the "
        + "[MODFLOW 6 Examples GitHub Repository]"
        + "(https://github.com/MODFLOW-USGS/modflow6-examples) are "
        + "used to compare run times. Simulations that fail are "
        + "indicated by '--'. The percent difference, where calculated, "
        + "is relative to the simulation run time for the previous "
        + "version. Percent differences for example problems with "
        + "short run times (less than 30 seconds) may not be significant.\n\n"
        + f"{get_mf6_compiler(current_app, verbose=True)}.\n\n\n"
    )
    line += "| Example Problem "
    line += f"| Current Version {v} "
    line += f"| Previous Version {v0} "
    line += "| Percent difference |\n"
    line += "| :---------- | :----------: | :----------: | :----------: |\n"
    f.write(line)

    #
    total_t = 0.0
    total_t0 = 0.0

    # get examples
    example_dirs = get_examples()

    # run models
    for idx, example in enumerate(example_dirs):
        success, t, t0 = run_model(
            current_app,
            previous_app,
            example,
            f,
            silent=False,
        )
        assert success, f"{example} run failed"
        total_t += t
        total_t0 += t0

    # add total
    pd = (total_t - total_t0) / total_t0

    # add final line
    line = f"| Total simulation time |"
    line += f" {elapsed_real_to_string(total_t)} |"
    line += f" {elapsed_real_to_string(total_t0)} |"
    line += f" {pd:.2%} |"
    f.write(f"{line}\n")
    f.flush()

    # close the markdown file
    f.close()
