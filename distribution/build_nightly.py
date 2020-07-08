import os
import sys
import platform
import shutil
import flopy
import pymake

# make sure exe extension is used on windows
eext = ''
soext = '.so'
if sys.platform.lower() == 'win32':
    eext = '.exe'
    soext = '.dll'

binpth, temppth = os.path.join('..', 'bin'), os.path.join('temp')

# some flags to check for errors in the code
# add -Werror for compilation to terminate if errors are found
strict_flags = ('-Wtabs -Wline-truncation -Wunused-label '
                '-Wunused-variable -pedantic -std=f2008')


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


def test_build_modflow6():
    # determine if app should be build
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--nomf6':
            txt = 'Command line cancel of MODFLOW 6 build'
            print(txt)
            return

    # set source and target paths
    srcdir = os.path.join('..', 'src')
    target = os.path.join('..', 'bin', 'mf6')
    target += eext
    fc, cc = pymake.set_compiler('mf6')

    fflags = None
    if fc == 'gfortran':
        fflags = strict_flags

    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags)

    msg = '{} does not exist.'.format(relpath_fallback(target))
    assert os.path.isfile(target), msg

    # return
    return


def test_build_modflow6_so():
    # determine if app should be build
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--nomf6so':
            txt = 'Command line cancel of MODFLOW 6 shared object build'
            print(txt)
            return

    # set source and target paths
    srcdir = os.path.join('..', 'srcbmi')
    comdir = os.path.join('..', 'src')
    excludefiles = [os.path.join(comdir, 'mf6.f90')]
    target = os.path.join('..', 'bin', 'libmf6')
    target += soext
    fc, cc = pymake.set_compiler('mf6')

    fflags = None
    if fc == 'gfortran':
        fflags = strict_flags

    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags, srcdir2=comdir, excludefiles=excludefiles,
                sharedobject=True)

    msg = '{} does not exist.'.format(relpath_fallback(target))
    assert os.path.isfile(target), msg

    # return
    return


def test_build_mf5to6():
    # determine if app should be build
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--nomf5to6':
            txt = 'Command line cancel of MODFLOW 5 to 6 converter build'
            print(txt)
            return

    # set source and target paths
    srcdir = os.path.join('..', 'utils', 'mf5to6', 'src')
    target = os.path.join('..', 'bin', 'mf5to6')
    target += eext
    extrafiles = os.path.join('..', 'utils', 'mf5to6', 'pymake',
                              'extrafiles.txt')
    fc, cc = pymake.set_compiler('mf6')

    # build modflow 5 to 6 converter
    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                extrafiles=extrafiles)

    msg = '{} does not exist.'.format(relpath_fallback(target))
    assert os.path.isfile(target), msg

    # return
    return


def test_build_zonebudget():
    # determine if app should be build
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--nozonebudget':
            txt = 'Command line cancel of ZONEBUDGET for MODFLOW 6 build'
            print(txt)
            return

    # set source and target paths
    srcdir = os.path.join('..', 'utils', 'zonebudget', 'src')
    target = os.path.join('..', 'bin', 'zbud6')
    target += eext
    extrafiles = os.path.join('..', 'utils', 'zonebudget', 'pymake',
                              'extrafiles.txt')
    fc, cc = pymake.set_compiler('mf6')

    fflags = None
    if fc == 'gfortran':
        fflags = strict_flags

    pymake.main(srcdir, target, fc=fc, cc=cc, extrafiles=extrafiles,
                fflags=fflags)

    msg = '{} does not exist.'.format(relpath_fallback(target))
    assert os.path.isfile(target), msg

    # return
    return


def test_update_mf6io():
    from mkdist import update_mf6io_tex_files
    if not os.path.isdir(temppth):
        os.makedirs(temppth)
    # build simple model
    name = 'mymodel'
    ws = os.path.join(temppth, name)
    exe_name = os.path.join(binpth, 'mf6')
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
    test_build_modflow6()
    test_build_modflow6_so()
    test_build_mf5to6()
    test_build_zonebudget()
    test_update_mf6io()
    test_zip_assets()
