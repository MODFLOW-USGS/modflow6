import os
import sys
import platform
import shutil
import flopy
import pymake

os.environ["TRAVIS"] = "1"

if 'TRAVIS' in os.environ:
    os.environ['PYMAKE_DOUBLE'] = '1'

# paths to executables for  previous versions of MODFLOW
ebindir = os.path.abspath(
    os.path.join(os.path.expanduser('~'), '.local', 'bin'))
if not os.path.exists(ebindir):
    os.makedirs(ebindir)

# make sure exe extension is used on windows
eext = ''
sysinfo = platform.system()
if sysinfo.lower() == 'windows':
    eext = '.exe'

download_version = '2.0'
mfexe_pth = 'temp/mfexes'


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

    return


def test_create_dirs():
    pths = [os.path.join('..', 'bin'),
            os.path.join('temp')]

    for pth in pths:
        create_dir(pth)

    return


def getmfexes(pth='.', version='', pltfrm=None):
    """
    Get the latest MODFLOW binary executables from a github site
    (https://github.com/MODFLOW-USGS/executables) for the specified
    operating system and put them in the specified path.

    Parameters
    ----------
    pth : str
        Location to put the executables (default is current working directory)

    version : str
        Version of the MODFLOW-USGS/executables release to use.

    pltfrm : str
        Platform that will run the executables.  Valid values include mac,
        linux, win32 and win64.  If platform is None, then routine will
        download the latest appropriate zipfile from the github repository
        based on the platform running this script.

    """

    # Determine the platform in order to construct the zip file name
    if pltfrm is None:
        if sys.platform.lower() == 'darwin':
            pltfrm = 'mac'
        elif sys.platform.lower().startswith('linux'):
            pltfrm = 'linux'
        elif 'win' in sys.platform.lower():
            is_64bits = sys.maxsize > 2 ** 32
            if is_64bits:
                pltfrm = 'win64'
            else:
                pltfrm = 'win32'
        else:
            errmsg = ('Could not determine platform'
                      '.  sys.platform is {}'.format(sys.platform))
            raise Exception(errmsg)
    else:
        assert pltfrm in ['mac', 'linux', 'win32', 'win64']
    zipname = '{}.zip'.format(pltfrm)

    # Determine path for file download and then download and unzip
    url = ('https://github.com/MODFLOW-USGS/executables/'
           'releases/download/{}/'.format(version))
    assets = {p: url + p for p in ['mac.zip', 'linux.zip',
                                   'win32.zip', 'win64.zip']}
    download_url = assets[zipname]
    pymake.download_and_unzip(download_url, pth, verify=False)

    return


def test_getmfexes():
    yield getmfexes, mfexe_pth, download_version
    return


def copy_app_in_mfexe(target):
    found = False
    if os.path.isdir(mfexe_pth):
        if target in os.listdir(mfexe_pth):
            srcpth = os.path.join(mfexe_pth, target)
            dstpth = os.path.join(ebindir, target)
            print('copying {} -> {}'.format(srcpth, dstpth))
            shutil.copy(srcpth, dstpth)
            found = True

    return found


def test_build_modflow():
    found = copy_app_in_mfexe('mf2005dbl' + eext)
    if not found:
        pymake.build_apps('mf2005')
    return


def test_build_mfnwt():
    found = copy_app_in_mfexe('mfnwtdbl' + eext)
    if not found:
        pymake.build_apps('mfnwt')
    return


def test_build_usg():
    found = copy_app_in_mfexe('mfusgdbl' + eext)
    if not found:
        pymake.build_apps('mfusg')


def test_build_lgr():
    found = copy_app_in_mfexe('mflgrdbl' + eext)
    if not found:
        pymake.build_apps('mflgr')
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
        # some flags to check for errors in the code
        # add -Werror for compilation to terminate if errors are found
        fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                  '-Wunused-variable -pedantic -std=f2008')
        #fflags = None

    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags)

    msg = '{} does not exist.'.format(relpath_fallback(target))
    assert os.path.isfile(target), msg


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
        # some flags to check for errors in the code
        # add -Werror for compilation to terminate if errors are found
        fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                  '-Wunused-variable -pedantic -std=f2008')
        #fflags = None

    pymake.main(srcdir, target, fc=fc, cc=cc, extrafiles=extrafiles,
                fflags=fflags)

    msg = '{} does not exist.'.format(relpath_fallback(target))
    assert os.path.isfile(target), msg


if __name__ == "__main__":
    #test_create_dirs()
    #getmfexes(pth=mfexe_pth, version=download_version)
    #test_build_modflow()
    #test_build_mfnwt()
    #test_build_usg()
    #test_build_lgr()
    test_build_modflow6()
    #test_build_mf5to6()
    #test_build_zonebudget()
