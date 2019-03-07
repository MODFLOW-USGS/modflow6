import os
import sys
import platform
import shutil
import flopy
import pymake


# update these for new versions
mf2005dir = 'MF2005.1_12u'
mf2005url = "https://water.usgs.gov/ogw/modflow/MODFLOW-2005_v1.12.00/{}.zip".format(mf2005dir)
mfnwtdir = 'MODFLOW-NWT_1.1.3'
mfnwturl = "https://water.usgs.gov/ogw/modflow-nwt/{0}.zip".format(mfnwtdir)
mfusgdir = 'mfusg1_5'
mfusgurl = 'https://water.usgs.gov/water-resources/software/MODFLOW-USG/{0}.zip'.format(mfusgdir)
mflgrdir = 'mflgr.2_0'
mflgrurl = 'https://water.usgs.gov/ogw/modflow-lgr/modflow-lgr-v2.0.0/mflgrv2_0_00.zip'

# paths to executables for  previous versions of MODFLOW
ebindir = os.path.abspath(os.path.join(os.path.expanduser('~'), '.local', 'bin'))

fc = 'gfortran'
cc = 'gcc'

# make sure exe extension is used on windows
eext = ''
sysinfo = platform.system()
if sysinfo.lower() == 'windows':
    eext = '.exe'


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

def set_compiler():
    fct = fc
    cct = cc
    # parse command line arguments to see if user specified options
    # relative to building the target
    msg = ''
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--ifort':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with ifort.'.format(starget)
            fct = 'ifort'
        elif arg.lower() == '--icc':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with cl.'.format(starget)
            cct = 'icc'
        elif arg.lower() == '--cl':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with cl.'.format(starget)
            cct = 'cl'
        elif arg.lower() == '--clang':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with clang.'.format(starget)
            cct = 'clang'
    if len(msg) > 0:
        print(msg)
        
    return fct, cct


def test_build_modflow():
    starget = 'MODFLOW-2005'

    fct, cct = set_compiler()

    # set up target
    target = os.path.abspath(os.path.join(ebindir, 'mf2005dbl'))
    target += eext
    
    rebuild = rebuild_exe(target, starget)
    if not rebuild:
        return

    # get current directory
    cpth = os.getcwd()

    # create temporary path
    dstpth = os.path.join('tempbin')
    print('create...{}'.format(dstpth))
    if not os.path.exists(dstpth):
        os.makedirs(dstpth)
    os.chdir(dstpth)

    # Set dir name
    dirname = mf2005dir
    srcdir = os.path.join(dirname, 'src')

    # Download the MODFLOW-2005 distribution
    url = mf2005url
    pymake.download_and_unzip(url)

    # compile code
    print('compiling...{}'.format(os.path.relpath(target)))
    pymake.main(srcdir, target, fct, cct, makeclean=True,
                expedite=False, dryrun=False, double=True, debug=False)

    msg = '{} does not exist.'.format(os.path.relpath(target))
    assert os.path.isfile(target), msg

    # change back to original path
    os.chdir(cpth)

    # Clean up downloaded directory
    print('delete...{}'.format(dstpth))
    if os.path.isdir(dstpth):
        shutil.rmtree(dstpth)

    return


def test_build_mfnwt():
    starget = 'MODFLOW-NWT'

    fct, cct = set_compiler()

    # set up target
    target = os.path.abspath(os.path.join(ebindir, 'mfnwtdbl'))
    target += eext
    
    rebuild = rebuild_exe(target, starget)
    if not rebuild:
        return

    # get current directory
    cpth = os.getcwd()

    # create temporary path
    dstpth = os.path.join('tempbin')
    print('create...{}'.format(dstpth))
    if not os.path.exists(dstpth):
        os.makedirs(dstpth)
    os.chdir(dstpth)

    # Set dir name
    dirname = mfnwtdir
    srcdir = os.path.join(dirname, 'src')

    # Download the MODFLOW-NWT distribution
    url = mfnwturl
    pymake.download_and_unzip(url)

    # compile code
    print('compiling...{}'.format(os.path.relpath(target)))
    pymake.main(srcdir, target, fct, cct, makeclean=True,
                expedite=False, dryrun=False, double=True, debug=False)

    msg = '{} does not exist.'.format(os.path.relpath(target))
    assert os.path.isfile(target), msg

    # change back to original path
    os.chdir(cpth)

    # Clean up downloaded directory
    print('delete...{}'.format(dstpth))
    if os.path.isdir(dstpth):
        shutil.rmtree(dstpth)

    return


def test_build_usg():
    starget = 'MODFLOW-USG'

    fct, cct = set_compiler()

    # set up target
    target = os.path.abspath(os.path.join(ebindir, 'mfusgdbl'))
    target += eext
    
    rebuild = rebuild_exe(target, starget)
    if not rebuild:
        return

    # get current directory
    cpth = os.getcwd()

    # create temporary path
    dstpth = os.path.join('tempbin')
    print('create...{}'.format(dstpth))
    if not os.path.exists(dstpth):
        os.makedirs(dstpth)
    os.chdir(dstpth)

    # Set dir name
    dirname = mfusgdir
    srcdir = os.path.join(dirname, 'src')

    # Download the MODFLOW-USG distribution
    url = mfusgurl
    pymake.download_and_unzip(url)

    # compile code
    print('compiling...{}'.format(os.path.relpath(target)))
    pymake.main(srcdir, target, fct, cct, makeclean=True,
                expedite=False, dryrun=False, double=True, debug=False)

    msg = '{} does not exist.'.format(os.path.relpath(target))
    assert os.path.isfile(target), msg

    # change back to original path
    os.chdir(cpth)

    # Clean up downloaded directory
    print('delete...{}'.format(dstpth))
    if os.path.isdir(dstpth):
        shutil.rmtree(dstpth)

    return


def test_build_lgr():
    starget = 'MODFLOW-LGR'

    fct, cct = set_compiler()

    # set up target
    target = os.path.abspath(os.path.join(ebindir, 'mflgrdbl'))
    target += eext
    
    rebuild = rebuild_exe(target, starget)
    if not rebuild:
        return

    # get current directory
    cpth = os.getcwd()

    # create temporary path
    dstpth = os.path.join('tempbin')
    print('create...{}'.format(dstpth))
    if not os.path.exists(dstpth):
        os.makedirs(dstpth)
    os.chdir(dstpth)

    # Set dir name
    dirname = mflgrdir
    srcdir = os.path.join(dirname, 'src')

    # Download the MODFLOW-LGR distribution
    url = mflgrurl
    pymake.download_and_unzip(url)

    # compile code
    print('compiling...{}'.format(os.path.relpath(target)))
    pymake.main(srcdir, target, fct, cct, makeclean=True,
                expedite=False, dryrun=False, double=True, debug=False)

    msg = '{} does not exist.'.format(os.path.relpath(target))
    assert os.path.isfile(target), msg

    # change back to original path
    os.chdir(cpth)

    # Clean up downloaded directory
    print('delete...{}'.format(dstpth))
    if os.path.isdir(dstpth):
        shutil.rmtree(dstpth)

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
    srcdir2 = None

    fflags = None
    fct, cct = set_compiler()
    if fct == 'gfortran':
        # some flags to check for errors in the code
        # but they are not working yet, so had to deactivate
        fflags = 'Werror Wtabs Wline-truncation'
        fflags = None

    build(srcdir, srcdir2, target, 'MODFLOW 6', fflags=fflags)

    msg = '{} does not exist.'.format(os.path.relpath(target))
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
    srcdir2 = None
    extrafiles = os.path.join('..', 'utils', 'mf5to6', 'pymake',
                              'extrafiles.txt')

    # build modflow 5 to 6 converter
    build(srcdir, srcdir2, target, 'MODFLOW 5 to 6 converter',
          extrafiles=extrafiles)

    msg = '{} does not exist.'.format(os.path.relpath(target))
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
    srcdir2 = None
    extrafiles = os.path.join('..', 'utils', 'zonebudget', 'pymake',
                              'extrafiles.txt')

    build(srcdir, srcdir2, target, 'ZONEBUDGET for MODFLOW 6',
          extrafiles=extrafiles)

    msg = '{} does not exist.'.format(os.path.relpath(target))
    assert os.path.isfile(target), msg


def rebuild_exe(target, starget):
    rebuild = True
    epth = os.path.basename(target)
    exe_exists = flopy.which(epth)
    if exe_exists is not None:
        print('No need to build {}'.format(starget) +
              ' since it exists in the current path')
        rebuild = False
    return rebuild


def build(srcdir, srcdir2, target, starget, extrafiles=None, fflags=None):
    """
    Build a specified target
    """
    debug = False

    fct, cct = set_compiler()

    # parse remaining command line arguments to see if user specified options
    # relative to building the target
    msg = ''
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--debug':
            debug = True
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with debug flags.'.format(starget)
        elif arg.lower() == '--fflags':
            if len(sys.argv) > idx + 1:
                if fflags is None:
                    fflags = ''
                else:
                    fflags += ' '
                t = sys.argv[idx + 1:]
                for tt in t:
                    fflags += tt + ' '
                break
    if len(msg) > 0:
        print(msg)

    # write message to log
    txt = 'checking if {} should be built'.format(starget)
    print(txt)
    # determine if executable should be built
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--nobuild':
            print('{} will not be built'.format(starget))
            return

    # make sure exe extension is used on windows
    sysinfo = platform.system()
    if sysinfo.lower() == 'windows':
        filename, fileext = os.path.splitext(target)
        if fileext.lower() != '.exe':
            target += '.exe'

    # call main -- note that this form allows main to be called
    # from python as a function.
    success = pymake.pymake.main(srcdir, target, fct, cct,
                                 include_subdirs=True,
                                 srcdir2=srcdir2,
                                 debug=debug, extrafiles=extrafiles,
                                 fflags=fflags)

    msg = 'Could not build {}'.format(target)
    assert success == 0, msg

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
    pymake.download_and_unzip(download_url, pth)

    return


def test_getmfexes():
    getmfexes('temp/mfexes', '1.0')
    return


if __name__ == "__main__":
    test_create_dirs()
    test_build_modflow()
    test_build_mfnwt()
    test_build_usg()
    test_build_lgr()
    test_build_modflow6()
    test_build_mf5to6()
    test_build_zonebudget()
    test_getmfexes()
