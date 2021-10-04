# Get executables

import os
import shutil
import pymake

from framework import running_on_CI

if running_on_CI():
    print("running on CI environment")
    os.environ["PYMAKE_DOUBLE"] = "1"

# paths to executables for previous versions of MODFLOW
ebindir = os.path.abspath(os.path.join(os.path.expanduser("~"), ".local", "bin"))
if not os.path.exists(ebindir):
    os.makedirs(ebindir)


mfexe_pth = "temp/mfexes"


def create_dir(pth):
    # remove pth directory if it exists
    if os.path.exists(pth):
        print("removing... {}".format(os.path.abspath(pth)))
        shutil.rmtree(pth)
    # create pth directory
    print("creating... {}".format(os.path.abspath(pth)))
    os.makedirs(pth)

    msg = "could not create... {}".format(os.path.abspath(pth))
    assert os.path.exists(pth), msg


def test_create_dirs():
    pths = [os.path.join("..", "bin"), os.path.join("temp")]

    for pth in pths:
        create_dir(pth)


def test_getmfexes(verify=True):
    pymake.getmfexes(mfexe_pth, verify=verify)
    for target in os.listdir(mfexe_pth):
        srcpth = os.path.join(mfexe_pth, target)
        if os.path.isfile(srcpth):
            dstpth = os.path.join(ebindir, target)
            print("copying {} -> {}".format(srcpth, dstpth))
            shutil.copy(srcpth, dstpth)


if __name__ == "__main__":
    test_create_dirs()
    test_getmfexes(verify=False)
