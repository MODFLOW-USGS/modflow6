# Get executables

import os
import shutil

import pymake

from build_exes import meson_build
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

    msg = f"could not create... {os.path.abspath(pth)}"
    assert os.path.exists(pth), msg


def rebuild_mf6_release():
    target = "mf6"
    download_pth = os.path.join("temp")
    target_dict = pymake.usgs_program_data.get_target(target)

    pymake.download_and_unzip(
        target_dict["url"],
        pth=download_pth,
        verbose=True,
    )

    # update IDEVELOP MODE in the release
    srcpth = os.path.join(
        download_pth, target_dict["dirname"], target_dict["srcdir"]
    )
    fpth = os.path.join(srcpth, "Utilities", "version.f90")
    with open(fpth) as f:
        lines = f.read().splitlines()
    assert len(lines) > 0, f"could not update {srcpth}"

    f = open(fpth, "w")
    for line in lines:
        tag = "IDEVELOPMODE = 0"
        if tag in line:
            line = line.replace(tag, "IDEVELOPMODE = 1")
        f.write(f"{line}\n")
    f.close()

    # build release source files with Meson
    root_path = os.path.join(download_pth, target_dict["dirname"])
    meson_build(dir_path=root_path, libdir=os.path.abspath(rebuilt_bindir))


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
            print(f"copying {srcpth} -> {dstpth}")
            shutil.copy(srcpth, dstpth)


def test_rebuild_mf6_release():
    rebuild_mf6_release()


if __name__ == "__main__":
    test_create_dirs()
    test_getmfexes(verify=False)
    test_rebuild_mf6_release()
