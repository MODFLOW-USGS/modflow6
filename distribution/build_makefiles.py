import os
import sys
from contextlib import contextmanager

import pymake

if sys.platform.lower() == "win32":
    ext = ".exe"
else:
    ext = ""

# check for command line arguments
fc = None
for idx, arg in enumerate(sys.argv):
    if arg in ("-fc",):
        fc = sys.argv[idx + 1]

# if compiler not set by command line argument
# use environmental variable or set to default compiler (gfortran)
if fc is None:
    if "FC" in os.environ:
        fc = os.getenv("FC")
    else:
        fc = "gfortran"


@contextmanager
def cwd(path):
    oldpwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(oldpwd)


def run_makefile(target):
    assert os.path.isfile(
        "makefile"
    ), f"makefile does not exist in {os.getcwd()}"

    base_target = os.path.basename(target)
    base_message = (
        f" Rerunning {os.path.basename(__file__)} in the distribution "
        "directory and recomitting modified makefiles will likely resolve "
        "CI failures."
    )

    # clean prior to make
    print(f"clean {base_target} with makefile")
    os.system("make clean")

    # build MODFLOW 6 with makefile
    print(f"build {base_target} with makefile")
    return_code = os.system(f"make FC={fc}")

    assert return_code == 0, f"could not make '{base_target}'." + base_message

    assert os.path.isfile(target), (
        f"{base_target} does not exist." + base_message
    )

    # clean after successful make
    print(f"clean {base_target} with makefile")
    os.system("make clean")

    return


def build_mf6_makefile():
    with cwd(os.path.join("..", "make")):
        pm = pymake.Pymake()
        pm.target = "mf6"
        pm.srcdir = os.path.join("..", "src")
        pm.appdir = os.path.join("..", "bin")
        pm.include_subdirs = True
        pm.inplace = True
        pm.dryrun = True
        pm.makefile = True
        pm.networkx = True

        # build the application
        pm.build()

        msg = f"could not create makefile for '{pm.target}'."
        assert pm.returncode == 0, msg

    return


def build_zbud6_makefile():
    with cwd(os.path.join("..", "utils", "zonebudget", "make")):
        pm = pymake.Pymake()
        pm.target = "zbud6"
        pm.srcdir = os.path.join("..", "src")
        pm.appdir = os.path.join("..", "..", "..", "bin")
        pm.extrafiles = os.path.join("..", "pymake", "extrafiles.txt")
        pm.inplace = True
        pm.makeclean = True
        pm.dryrun = True
        pm.makefile = True
        pm.networkx = True

        # build the application
        pm.build()

        msg = f"could not create makefile for '{pm.target}'."
        assert pm.returncode == 0, msg

    return


def build_mf5to6_makefile():
    with cwd(os.path.join("..", "utils", "mf5to6", "make")):
        srcdir = os.path.join("..", "src")
        target = os.path.join("..", "..", "..", "bin", "mf5to6")
        extrafiles = os.path.join("..", "pymake", "extrafiles.txt")

        # build modflow 5 to 6 converter
        returncode = pymake.main(
            srcdir,
            target,
            include_subdirs=True,
            extrafiles=extrafiles,
            inplace=True,
            dryrun=True,
            makefile=True,
            networkx=True,
            fflags="-fall-intrinsics",
        )

        msg = f"could not create makefile for '{os.path.basename(target)}'."
        assert returncode == 0, msg

    return


def test_build_mf6_wmake():
    target = os.path.join("..", "bin", f"mf6{ext}")
    with cwd(os.path.join("..", "make")):
        run_makefile(target)


def test_build_zbud6_wmake():
    target = os.path.join("..", "..", "..", "bin", f"zbud6{ext}")
    with cwd(os.path.join("..", "utils", "zonebudget", "make")):
        run_makefile(target)


def test_build_mf5to6_wmake():
    target = os.path.join("..", "..", "..", "bin", f"mf5to6{ext}")
    with cwd(os.path.join("..", "utils", "mf5to6", "make")):
        run_makefile(target)


if __name__ == "__main__":
    build_mf6_makefile()
    build_zbud6_makefile()
    build_mf5to6_makefile()
    # test_build_mf6_wmake()
    # test_build_zbud6_wmake()
    # test_build_mf5to6_wmake()
