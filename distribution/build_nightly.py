import os
import sys
import platform
import shutil
import flopy
import pymake

# add path to build script in autotest directory and reuse mf6 build scripts
sys.path.append(os.path.join("..", "autotest"))
from get_build_exes import build_mf6, build_mf6_so, build_mf5to6, build_zbud6

# make sure exe extension is used on windows
eext = ''
soext = '.so'
if sys.platform.lower() == 'win32':
    eext = '.exe'
    soext = '.dll'

binpth, temppth = os.path.join('..', 'bin'), os.path.join('temp')


def get_zipname():
    zipname = sys.platform.lower()
    if zipname == "linux2":
        zipname = "linux"
    elif zipname == "darwin":
        zipname = "mac"
    elif zipname == "win32":
        if platform.architecture()[0] == "64bit":
            zipname = "win64"

    # return
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
        print('removing... {}'.format(os.path.abspath(pth)))
        shutil.rmtree(pth)

    # create pth directory
    print('creating... {}'.format(os.path.abspath(pth)))
    os.makedirs(pth)

    msg = 'could not create... {}'.format(os.path.abspath(pth))
    assert os.path.exists(pth), msg

    # return
    return


def test_update_version():
    from make_release import update_version
    update_version()

    # return
    return


def test_create_dirs():
    pths = [binpth, temppth]

    for pth in pths:
        create_dir(pth)

    # return
    return


def test_mf6():
    build_mf6()


def test_libmf6():
    build_mf6_so()


def test_mf5to6():
    build_mf5to6()


def test_zbud6():
    build_zbud6()


def test_update_mf6io():
    from mkdist import update_mf6io_tex_files
    if not os.path.isdir(temppth):
        os.makedirs(temppth)
    # build simple model
    name = 'mymodel'
    ws = os.path.join(temppth, name)
    exe_name = 'mf6'
    if sys.platform.lower() == 'win32':
        exe_name += '.exe'
    exe_name = os.path.join(binpth, exe_name)
    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws, exe_name=exe_name)
    tdis = flopy.mf6.ModflowTdis(sim)
    ims = flopy.mf6.ModflowIms(sim)
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)
    dis = flopy.mf6.ModflowGwfdis(gwf, nrow=10, ncol=10)
    ic = flopy.mf6.ModflowGwfic(gwf)
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_specific_discharge=True)
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=[[(0, 0, 0), 1.],
                                                           [(0, 9, 9), 0.]])
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                printrecord=[('BUDGET', 'ALL')])
    sim.write_simulation()

    # update the mf6io simulation output for LaTeX
    update_mf6io_tex_files(None, exe_name, expth=ws)

    # return
    return


def test_zip_assets():
    # create temppth if it does not exist
    if not os.path.isdir(temppth):
        os.makedirs(temppth)

    # zip assets
    env = 'GITHUB_ACTIONS'
    os.environ[env] = "true"
    if env in os.environ:
        fpth = get_zipname() + '.zip'
        # zip up exe's using directories
        zip_pth = os.path.join(temppth, fpth)
        success = pymake.zip_all(zip_pth, dir_pths=binpth)
        assert success, "could not create '{}'".format(zip_pth)
    return


if __name__ == "__main__":
    test_update_version()
    test_create_dirs()
    test_mf6()
    test_libmf6()
    test_mf5to6()
    test_zbud6()
    test_update_mf6io()
    test_zip_assets()
