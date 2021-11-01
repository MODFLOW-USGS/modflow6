# Get executables

import os
import shutil
import pymake

from framework import running_on_CI

if running_on_CI():
    print("running on CI environment")
    os.environ["PYMAKE_DOUBLE"] = "1"

# path to rebuilt executables for previous versions of MODFLOW
rebuilt_bindir = os.path.join("..", "bin", "rebuilt")

if not os.path.exists(rebuilt_bindir):
    os.makedirs(rebuilt_bindir)

# paths to downloaded for previous versions of MODFLOW
downloaded_bindir = os.path.join("..", "bin", "downloaded")

if not os.path.exists(downloaded_bindir):
    os.makedirs(downloaded_bindir)


mfexe_pth = "temp/mfexes"

# use the line below to set fortran compiler using environmental variables
# os.environ["FC"] = "gfortran"

# some flags to check for errors in the code
# add -Werror for compilation to terminate if errors are found
strict_flags = (
    "-Wtabs -Wline-truncation -Wunused-label "
    "-Wunused-variable -pedantic -std=f2008 "
    "-Wcharacter-truncation"
)


def get_compiler_envvar(fc):
    env_var = os.environ.get("FC")
    if env_var is not None:
        if env_var != fc:
            fc = env_var
    return fc


def create_dir(pth):
    # create pth directory
    print(f"creating... {os.path.abspath(pth)}")
    os.makedirs(pth, exist_ok=True)

    msg = "could not create... {}".format(os.path.abspath(pth))
    assert os.path.exists(pth), msg


def rebuild_mf6_release():
    pm = pymake.Pymake(verbose=True)
    pm.target = "mf6"
    pm.appdir = rebuilt_bindir
    download_pth = os.path.join("temp")
    target_dict = pymake.usgs_program_data.get_target(pm.target)

    pm.download_target(pm.target, download_path=download_pth, verify=False)

    # Set MODFLOW 6 to compile develop version of the release
    srcpth = os.path.join(
        download_pth, target_dict["dirname"], target_dict["srcdir"]
    )
    fpth = os.path.join(srcpth, "Utilities", "version.f90")
    with open(fpth) as f:
        lines = f.read().splitlines()

    assert len(lines) > 0, "could not update {}".format(srcpth)

    f = open(fpth, "w")
    for line in lines:
        tag = "IDEVELOPMODE = 0"
        if tag in line:
            line = line.replace(tag, "IDEVELOPMODE = 1")
        f.write("{}\n".format(line))
    f.close()

    # reset compiler based on environmental variable, if defined
    pm.fc = get_compiler_envvar(pm.fc)

    # add strict flags if gfortran is being used
    if pm.fc == "gfortran":
        pm.fflags = strict_flags

    # build the release version of MODFLOW 6
    pm.build()

    msg = "{} does not exist.".format(pm.target)
    assert pm.returncode == 0, msg

    # finalize the build
    pm.finalize()


def test_create_dirs():
    pths = [os.path.join("..", "bin"), os.path.join("temp")]

    for pth in pths:
        create_dir(pth)


def test_getmfexes(verify=True):
    pymake.getmfexes(mfexe_pth, verify=verify)
    for target in os.listdir(mfexe_pth):
        srcpth = os.path.join(mfexe_pth, target)
        if os.path.isfile(srcpth):
            dstpth = os.path.join(downloaded_bindir, target)
            print("copying {} -> {}".format(srcpth, dstpth))
            shutil.copy(srcpth, dstpth)


def test_rebuild_mf6_release():
    rebuild_mf6_release()


if __name__ == "__main__":
    test_create_dirs()
    test_getmfexes(verify=False)
