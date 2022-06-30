# Build targets

# to use ifort on windows, run this
# python build_exes.py -fc ifort

# can compile only mf6 directly using this command:
#  python -c "import build_exes; build_exes.test_build_modflow6()"

import os
import pathlib as pl
import subprocess as sp
import sys
from contextlib import contextmanager

from framework import running_on_CI

if running_on_CI():
    print("running on CI environment")
    os.environ["PYMAKE_DOUBLE"] = "1"

# set OS dependent extensions
eext = ""
soext = ".so"
if sys.platform.lower() == "win32":
    eext = ".exe"
    soext = ".dll"
elif sys.platform.lower() == "darwin":
    soext = ".dylib"

mfexe_pth = "temp/mfexes"

# use the line below to set fortran compiler using environmental variables
# os.environ["FC"] = "ifort"

# some flags to check for errors in the code
# add -Werror for compilation to terminate if errors are found
strict_flags = (
    "-fall-intrinsics "
    "-Wtabs -Wline-truncation -Wunused-label "
    "-Wunused-variable -pedantic -std=f2008 "
    "-Wcharacter-truncation"
)


@contextmanager
def set_directory(path: str):
    """Sets the cwd within the context

    Args:
        path (Path): The path to the cwd

    Yields:
        None
    """

    origin = os.path.abspath(os.getcwd())
    path = os.path.abspath(path)
    try:
        os.chdir(path)
        print(f"change from {origin} -> {path}")
        yield
    finally:
        os.chdir(origin)
        print(f"change from {path} -> {origin}")


def relpath_fallback(pth):
    try:
        # throws ValueError on Windows if pth is on a different drive
        return os.path.relpath(pth)
    except ValueError:
        return os.path.abspath(pth)


def create_dir(pth):
    # create pth directory
    print(f"creating... {os.path.abspath(pth)}")
    os.makedirs(pth, exist_ok=True)

    msg = f"could not create... {os.path.abspath(pth)}"
    assert os.path.exists(pth), msg


def set_compiler_environment_variable():
    fc = None

    # parse command line arguments
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == "-fc":
            fc = sys.argv[idx + 1]
        elif arg.lower().startswith("-fc="):
            fc = arg.split("=")[1]

    # determine if fc needs to be set to the FC environmental variable
    env_var = os.getenv("FC", default="gfortran")
    if fc is None and fc != env_var:
        fc = env_var

    # validate Fortran compiler
    fc_options = (
        "gfortran",
        "ifort",
    )
    if fc not in fc_options:
        raise ValueError(
            f"Fortran compiler {fc} not supported. Fortran compile must be "
            + f"[{', '.join(str(value) for value in fc_options)}]."
        )

    # set FC environment variable
    os.environ["FC"] = fc


def meson_build(
    dir_path: str = "..",
    libdir: str = "bin",
):
    set_compiler_environment_variable()
    is_windows = sys.platform.lower() == "win32"
    with set_directory(dir_path):
        cmd = (
            "meson setup builddir "
            + f"--bindir={os.path.abspath(libdir)} "
            + f"--libdir={os.path.abspath(libdir)} "
            + "--prefix="
        )
        if is_windows:
            cmd += "%CD%"
        else:
            cmd += "$(pwd)"
        if pl.Path("builddir").is_dir():
            cmd += " --wipe"
        print(f"setup meson\nrunning...\n  {cmd}")
        sp.run(cmd, shell=True, check=True)

        cmd = "meson install -C builddir"
        print(f"build and install with meson\nrunning...\n  {cmd}")
        sp.run(cmd, shell=True, check=True)


def test_create_dirs():
    pths = [os.path.join("..", "bin"), os.path.join("temp")]

    for pth in pths:
        create_dir(pth)

    return


def test_meson_build():
    meson_build()


if __name__ == "__main__":
    test_create_dirs()
    test_meson_build()
