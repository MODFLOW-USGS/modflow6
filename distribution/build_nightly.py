import os
import pathlib
import platform
import shutil
import sys

import flopy
import pymake

# add path to build script in autotest directory and reuse mf6 build scripts
sys.path.append(os.path.join("..", "autotest"))
from build_exes import meson_build

# make sure exe extension is used on windows
eext = ""
soext = ".so"
if sys.platform.lower() == "win32":
    eext = ".exe"
    soext = ".dll"

bin_path = os.path.abspath(os.path.join("..", "bin"))
example_path = os.path.abspath(os.path.join("temp"))
zip_path = os.path.abspath(os.path.join("temp_zip"))


def get_zipname():
    zipname = sys.platform.lower()
    if zipname == "linux2":
        zipname = "linux"
    elif zipname == "darwin":
        zipname = "mac"
    elif zipname == "win32":
        if platform.architecture()[0] == "64bit":
            zipname = "win64"
    return zipname


def relpath_fallback(pth):
    try:
        # throws ValueError on Windows if pth is on a different drive
        return os.path.relpath(pth)
    except ValueError:
        return os.path.abspath(pth)


def create_dir(pth):
    # remove pth directory if it exists
    if os.path.exists(pth):
        print(f"removing... {os.path.abspath(pth)}")
        shutil.rmtree(pth)

    # create pth directory
    print(f"creating... {os.path.abspath(pth)}")
    os.makedirs(pth)

    msg = f"could not create... {os.path.abspath(pth)}"
    assert os.path.exists(pth), msg


def test_update_version():
    from make_release import update_version

    update_version()


def test_create_dirs():
    for pth in (
        bin_path,
        zip_path,
    ):
        create_dir(pth)


def test_nightly_build():
    meson_build()

    # test if there are any executable files to zip
    binpth_files = [
        os.path.join(bin_path, f)
        for f in os.listdir(bin_path)
        if os.path.isfile(os.path.join(bin_path, f))
        and shutil.which(os.path.join(bin_path, f), mode=os.X_OK)
        and pathlib.Path(os.path.join(bin_path, f)).suffix
        not in (".a", ".lib", ".pdb")
    ]
    if len(binpth_files) < 1:
        raise FileNotFoundError(
            f"No executable files present in {os.path.abspath(bin_path)}.\n"
            + f"Available files:\n [{', '.join(os.listdir(bin_path))}]"
        )
    else:
        print(f"Files to zip:\n [{', '.join(binpth_files)}]")

    zip_pth = os.path.abspath(os.path.join(zip_path, get_zipname() + ".zip"))
    print(f"Zipping files to '{zip_pth}'")
    success = pymake.zip_all(zip_pth, file_pths=binpth_files)
    assert success, f"Could not create '{zip_pth}'"


def test_update_mf6io():
    from mkdist import update_mf6io_tex_files

    # build simple model
    name = "mymodel"
    ws = os.path.join(example_path, name)
    exe_name = "mf6"
    if sys.platform.lower() == "win32":
        exe_name += ".exe"
    exe_name = os.path.join(bin_path, exe_name)
    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws, exe_name=exe_name)
    tdis = flopy.mf6.ModflowTdis(sim)
    ims = flopy.mf6.ModflowIms(sim)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(gwf, nrow=10, ncol=10)
    ic = flopy.mf6.ModflowGwfic(gwf)
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_specific_discharge=True)
    chd = flopy.mf6.ModflowGwfchd(
        gwf, stress_period_data=[[(0, 0, 0), 1.0], [(0, 9, 9), 0.0]]
    )
    oc = flopy.mf6.ModflowGwfoc(gwf, printrecord=[("BUDGET", "ALL")])
    sim.write_simulation()

    # update the mf6io simulation output for LaTeX
    update_mf6io_tex_files(None, exe_name, expth=ws)


if __name__ == "__main__":
    test_update_version()
    test_create_dirs()
    test_nightly_build()
    test_update_mf6io()
