"""
Python code to create a MODFLOW 6 distribution

To make a distribution:
  1.  update version in doc/version.tex
  2.  run this script, mkdist.py
  3.  run doc/ReleaseNotes/mk_example_items.py
  4.  run doc/ReleaseNotes/mk_example_table.py
  5.  run doc/ReleaseNotes/mk_folder_struct.py
  6.  use latex to build ReleaseNotes.pdf
  7.  run this script again

"""


import os
import sys
import shutil
import subprocess
import zipfile
import pymake
from pymake.download import download_and_unzip
from contextlib import contextmanager


@contextmanager
def cwd(path):
    oldpwd=os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(oldpwd)


def get_distribution_info(versiontexname):
    vername = None
    verdate = None
    fname = versiontexname
    with open(fname) as f:
        lines = f.readlines()
        f.close()
    for line in lines:
        # \newcommand{\modflowversion}{mf6beta0.9.00}
        srchtxt = 'modflowversion'
        if srchtxt in line:
            istart = line.rfind('{') + 1
            istop = line.rfind('}')
            if 0 < istart < istop:
                vername = line[istart: istop]
        srchtxt = 'modflowdate'
        if srchtxt in line:
            istart = line.rfind('{') + 1
            istop = line.rfind('}')
            if 0 < istart < istop:
                verdate = line[istart: istop]
        if verdate is not None:
            break
    return vername, verdate


def zipdir(dirname, zipname):
    print('Zipping directory: {}'.format(dirname))
    zipf = zipfile.ZipFile(zipname, 'w', zipfile.ZIP_DEFLATED)
    for root, dirs, files in os.walk(dirname):
        for file in files:
            if '.DS_Store' not in file:
                fname = os.path.join(root, file)
                print('  Adding to zip: ==> ', fname)
                zipf.write(fname, arcname=fname)
    zipf.close()
    print('\n')
    return


def setup(name, destpath, version, subdirs):
    """
    Setup the folder structure, and return a dictionary of subfolder name
    and the full path in destpath.

    """
    print(2 * '\n')
    print('Setting up {} distribution: {}'.format(name, version))
    print('\n')

    dest = os.path.join(destpath, version)
    if os.path.exists(dest):
        # Raise Exception('Destination path exists.  Kill it first.')
        print('Clobbering destination directory: {}'.format(dest))
        print('\n')
        shutil.rmtree(dest)
    os.mkdir(dest)

    print('Creating subdirectories')
    folderdict = {}
    for sd in subdirs:
        fullpath = os.path.join(dest, sd)
        print('  creating ==> {}'.format(fullpath))
        os.mkdir(fullpath)
        folderdict[sd] = fullpath
    print('\n')

    return folderdict


def copytree(src, dst, symlinks=False, ignore=None):
    """
    Copy a folder from src to dst.  If dst does not exist, then create it.

    """
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            print('  copying {} ===> {}'.format(s, d))
            shutil.copytree(s, d, symlinks, ignore)
        else:
            print('  copying {} ===> {}'.format(s, d))
            shutil.copy2(s, d)
    return


def convert_line_endings(folder, windows=True):
    """
    Convert all of the line endings to windows or unix

    """
    # Prior to zipping, enforce os line endings on all text files
    print('Converting line endings...')
    platform = sys.platform
    cmd = None
    if platform.lower() == 'darwin':
        if windows:
            cmd = "find . -name '*' | xargs unix2dos"
        else:
            cmd = "find . -name '*' | xargs dos2unix"
    else:
        if windows:
            cmd = 'for /R %G in (*) do unix2dos "%G"'
        else:
            cmd = 'for /R %G in (*) do dos2unix "%G"'
    p = subprocess.Popen(cmd, cwd=folder, shell=True)
    print(p.communicate())
    print('\n')
    return


def change_version_module(fname, version):
    """
    Update the version.f90 source code with the updated version number
    and turn develop mode off.

    """
    with open(fname) as f:
        lines = f.readlines()
    newlines = []
    found1 = False
    found2 = False
    for line in lines:
        newline = line
        srchtxt = 'character(len=40), parameter :: VERSION'
        if srchtxt in line:
            newline = "{} = '{}'".format(srchtxt, version)
            found1 = True
        srchtxt = 'integer(I4B), parameter :: IDEVELOPMODE'
        if srchtxt in line:
            newline = '{} = {}'.format(srchtxt, 0)
            found2 = True
        newlines.append(newline)
    if not found1 or not found2:
        raise Exception('could not replace version or developmode in source code')
    with open(fname, 'w') as f:
        for line in newlines:
            f.write(line.strip() + '\n')
    return


def make_zonebudget(srcpath, destpath, win_target_os, exepath):
    """
    Add zone budget to the distribution

    srcpath should be '../utils/zonebudget'
    destpath should be 'utils'
    sourcepath


    """

    # setup the folder structure
    name = 'zonebudget'
    version = 'zonebudget'
    subdirs = ['src', 'make', 'msvs']
    fd = setup(name, destpath, version, subdirs)

    # copy source folder
    sourcepath = os.path.join(srcpath, 'src')
    copytree(sourcepath, fd['src'], ignore=shutil.ignore_patterns('.DS_Store'))

    # Create makefile
    print('Creating zonebudget makefile')
    shutil.copyfile(os.path.join(srcpath, 'pymake', 'extrafiles.txt'),
                    os.path.join(fd['make'], 'extrafiles.txt'))
    with cwd(fd['make']):
        pymake.main(os.path.join('..', 'src'), 'zbud6', 'gfortran', 'gcc',
                    makeclean=True, dryrun=True, include_subdirs=True,
                    makefile=True, extrafiles='extrafiles.txt')
    os.remove(os.path.join(fd['make'], 'extrafiles.txt'))

    # Copy the Visual Studio project file
    flist = [os.path.join(srcpath, 'msvs', 'zonebudget.vfproj')]
    print('Copying zonebudget msvs files')
    for d in flist:
        print('  {} ===> {}'.format(d, fd['msvs']))
        shutil.copy(d, fd['msvs'])
    print('\n')

    # build the executable
    exename = 'zbud6'
    target = os.path.join(exepath, exename)
    if win_target_os:
        fc = 'ifort'
        cc = 'cl'
        exename += '.exe'
    else:
        fc = 'gfortran'
        cc = 'gcc'
    extrafiles = os.path.join(srcpath, 'pymake', 'extrafiles.txt')
    pymake.main(fd['src'], target, fc, cc, makeclean=True,
                include_subdirs=True, extrafiles=extrafiles)
    if win_target_os:
        target += '.exe'
    if not os.path.isfile(target):
        raise Exception('Did not build target: {}'.format(target))

    return


def make_mf5to6(srcpath, destpath, win_target_os, exepath):
    """
    Add mf5to6 to the distribution

    srcpath should be '../utils/mf5to6'
    destpath should be 'utils'
    sourcepath


    """

    # setup the folder structure
    name = 'mf5to6'
    version = 'mf5to6'
    subdirs = ['src', 'make', 'msvs']
    fd = setup(name, destpath, version, subdirs)

    # copy source folder
    sourcepath = os.path.join(srcpath, 'src')
    copytree(sourcepath, fd['src'], ignore=shutil.ignore_patterns('.DS_Store'))

    # Create makefile
    print('Creating mf5to6 makefile')
    shutil.copyfile(os.path.join(srcpath, 'pymake', 'extrafiles.txt'),
                    os.path.join(fd['make'], 'extrafiles.txt'))
    with cwd(fd['make']):
        pymake.main(os.path.join('..', 'src'), name, 'gfortran', 'gcc',
                    makeclean=True, dryrun=True, include_subdirs=True,
                    makefile=True, extrafiles='extrafiles.txt')
    os.remove(os.path.join(fd['make'], 'extrafiles.txt'))

    # Copy the Visual Studio project file
    flist = [os.path.join(srcpath, 'msvs', 'mf5to6.vfproj')]
    print('Copying mf5to6 msvs files')
    for d in flist:
        print('  {} ===> {}'.format(d, fd['msvs']))
        shutil.copy(d, fd['msvs'])
    print('\n')

    # build the executable
    exename = 'mf5to6'
    target = os.path.join(exepath, exename)
    if win_target_os:
        fc = 'ifort'
        cc = 'cl'
        exename += '.exe'
    else:
        fc = 'gfortran'
        cc = 'gcc'
    extrafiles = os.path.join(srcpath, 'pymake', 'extrafiles.txt')
    pymake.main(fd['src'], target, fc, cc, makeclean=True,
                include_subdirs=True, extrafiles=extrafiles)
    if win_target_os:
        target += '.exe'
    if not os.path.isfile(target):
        raise Exception('Did not build target: {}'.format(target))

    return


def delete_files(files, pth, allow_failure=False):
    for file in files:
        fpth = os.path.join(pth, file)
        try:
            print('removing...{}'.format(file))
            os.remove(fpth)
        except:
            print('could not remove...{}'.format(file))
            if not allow_failure:
                return False
    return True


def run_command(argv, pth, timeout=10):
    buff = ''
    ierr = 0
    with subprocess.Popen(argv,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT,
                          cwd=pth) as process:
        try:
            output, unused_err = process.communicate(timeout=timeout)
            buff = output.decode('utf-8')
        except subprocess.TimeoutExpired:
            process.kill()
            output, unused_err = process.communicate()
            buff = output.decode('utf-8')
            ierr = 100
        except:
            output, unused_err = process.communicate()
            buff = output.decode('utf-8')
            ierr = 101

    return buff, ierr


def clean_latex_files():

    print('Cleaning latex files')
    exts = ['pdf', 'aux', 'bbl', 'idx',
            'lof', 'out', 'toc']
    pth = os.path.join('..', 'doc', 'mf6io')
    files = ['mf6io.{}'.format(e) for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + '.pdf')

    pth = os.path.join('..', 'doc', 'ReleaseNotes')
    files = ['ReleaseNotes.{}'.format(e) for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + '.pdf')

    pth = os.path.join('..', 'doc', 'zonebudget')
    files = ['zonebudget.{}'.format(e) for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + '.pdf')

    pth = os.path.join('..', 'doc', 'ConverterGuide')
    files = ['converter_mf5to6.{}'.format(e) for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + '.pdf')

    return


def update_mf6io_tex_files(distfolder):

    texpth = '../doc/mf6io'
    fname1 = os.path.join(texpth, 'mf6output.tex')
    fname2 = os.path.join(texpth, 'mf6noname.tex')
    mf6pth = os.path.join(distfolder, 'bin') + 'mf6.exe'
    mf6pth = os.path.abspath(mf6pth)
    expth = os.path.join(distfolder, 'examples', 'ex01-twri')
    expth = os.path.abspath(expth)

    assert os.path.isfile(mf6pth)
    assert os.path.isdir(expth)

    # run an example model
    if os.path.isdir('./temp'):
        shutil.rmtree('./temp')
    shutil.copytree(expth, './temp')
    cmd = [os.path.abspath(mf6pth)]
    buff, ierr = run_command(cmd, './temp')
    lines = buff.split('\r\n')
    with open(fname1, 'w') as f:
        f.write('{}\n'.format('{\\small'))
        f.write('{}\n'.format('\\begin{lstlisting}[style=modeloutput]'))
        for line in lines:
            f.write(line.rstrip() + '\n')
        f.write('{}\n'.format('\\end{lstlisting}'))
        f.write('{}\n'.format('}'))

    # run model without a namefile present
    if os.path.isdir('./temp'):
        shutil.rmtree('./temp')
    os.mkdir('./temp')
    cmd = [os.path.abspath(mf6pth)]
    buff, ierr = run_command(cmd, './temp')
    lines = buff.split('\r\n')
    with open(fname2, 'w') as f:
        f.write('{}\n'.format('{\\small'))
        f.write('{}\n'.format('\\begin{lstlisting}[style=modeloutput]'))
        for line in lines:
            f.write(line.rstrip() + '\n')
        f.write('{}\n'.format('\\end{lstlisting}'))
        f.write('{}\n'.format('}'))

    # clean up
    if os.path.isdir('./temp'):
        shutil.rmtree('./temp')

    return


def build_latex_docs():
    print('Building latex files')
    pth = os.path.join('..', 'doc')
    doclist = [('mf6io', 'mf6io.tex'),
               ('ReleaseNotes', 'ReleaseNotes.tex'),
               ('zonebudget', 'zonebudget.tex'),
               ('ConverterGuide', 'converter_mf5to6.tex')]

    for d, t in doclist:

        dirname = os.path.join(pth, d)
        with cwd(dirname):

            cmd = ['pdflatex', t]
            buff, ierr = run_command(cmd, './')
            msg = '\nERROR {}: could not run {} on {}'.format(ierr, cmd[0],
                                                              cmd[1])
            assert ierr == 0, buff + msg

            cmd = ['bibtex', os.path.splitext(t)[0] + '.aux']
            buff, ierr = run_command(cmd, './')
            msg = '\nERROR {}: could not run {} on {}'.format(ierr, cmd[0],
                                                              cmd[1])
            assert ierr == 0, buff + msg

            cmd = ['pdflatex', t]
            buff, ierr = run_command(cmd, './')
            msg = '\nERROR {}: could not run {} on {}'.format(ierr, cmd[0],
                                                              cmd[1])
            assert ierr == 0, buff + msg

            cmd = ['pdflatex', t]
            buff, ierr = run_command(cmd, './')
            msg = '\nERROR {}: could not run {} on {}'.format(ierr, cmd[0],
                                                              cmd[1])
            assert ierr == 0, buff + msg

            fname = os.path.splitext(t)[0] + '.pdf'
            assert os.path.isfile(fname), 'Could not find ' + fname

    return


def update_latex_releaseinfo():

    pth = os.path.join('..', 'doc', 'ReleaseNotes')
    files = ['example_items.tex', 'example_table.tex', 'folder_struct.tex']
    delete_files(files, pth, allow_failure=True)

    cmd = ['python', 'mk_example_items.py']
    buff, ierr = run_command(cmd, pth)
    assert ierr == 0, buff + msg

    cmd = ['python', 'mk_example_table.py']
    buff, ierr = run_command(cmd, pth)
    assert ierr == 0, buff + msg

    cmd = ['python', 'mk_folder_struct.py']
    buff, ierr = run_command(cmd, pth)
    assert ierr == 0, buff + msg

    for f in files:
        assert os.path.isfile(os.path.join(pth, f)), 'File does not exist: ' + f

    return


if __name__ == '__main__':

    # setup paths and folder structure
    win_target_os = True
    name = 'MODFLOW 6'
    exename = 'mf6'
    destpath = '.'
    versiontexname = os.path.join('..', 'doc', 'version.tex')
    version, versiondate =  get_distribution_info(versiontexname)
    distfolder = os.path.join(destpath, version)
    subdirs = ['bin', 'doc', 'examples', 'src', 'msvs', 'make', 'utils']
    fd = setup(name, destpath, version, subdirs)

    # Copy the Visual Studio solution and project files
    flist = [
             # os.path.join('..', 'msvs', 'mf6.sln'),
             os.path.join('..', 'msvs', 'mf6.vfproj'),
             ]
    print('Copying msvs files')
    for d in flist:
        print('  {} ===> {}'.format(d, fd['msvs']))
        shutil.copy(d, fd['msvs'])
    print('\n')

    # copy source folder
    copytree(os.path.join('..', 'src'), fd['src'],
             ignore=shutil.ignore_patterns('.DS_Store'))

    # modify the constants fortran source file with version information
    fname = os.path.join(distfolder, 'src', 'Utilities', 'version.f90')
    change_version_module(fname, '{} {}'.format(version, versiondate))

    # Create makefile
    print('Creating makefile')
    with cwd(fd['make']):
        pymake.main(os.path.join('..', 'src'), 'mf6', 'gfortran', 'gcc',
                    makeclean=True, dryrun=True, include_subdirs=True,
                    makefile=True, extrafiles=None)

    # build MODFLOW 6 executable
    srcdir = fd['src']
    target = os.path.join(fd['bin'], exename)
    if win_target_os:
        fc = 'ifort'
        cc = 'cl'
    else:
        fc = 'gfortran'
        cc = 'gcc'
    pymake.main(srcdir, target, fc, cc, makeclean=True, include_subdirs=True)
    if win_target_os:
        target += '.exe'
    if not os.path.isfile(target):
        raise Exception('Did not build target: {}'.format(target))

    # setup zone budget
    make_zonebudget(os.path.join('..', 'utils', 'zonebudget'), fd['utils'],
                    win_target_os, fd['bin'])

    # setup mf5to6
    make_mf5to6(os.path.join('..', 'utils', 'mf5to6'), fd['utils'],
                win_target_os, fd['bin'])

    # examples
    expath = fd['examples']
    exsrcpath = os.path.join('..', '..', 'modflow6-examples.git', 'mf6')
    assert os.path.isdir(exsrcpath)
    examplelist = [
        ['test021_twri', 'twri'],
        ['test005_advgw_tidal', 'tidal'],
        ['test004_bcfss', 'bcf2ss'],
        ['test035_fhb', 'fhb'],

        ['test006_gwf3_gnc',  'mfusg1disu'],       # disu
        ['test006_gwf3_disv', 'mfusg1disv'],       # disv
        ['test006_2models_gnc',   'mfusg1lgr'],    # dis lgr
        ['test006_gwf3_disv_xt3d', 'mfusg1xt3d'],  # disv xt3d

        ['test041_flowdivert', 'bump'],
        ['test041_flowdivert_nr', 'bumpnr'],

        ['test050_circle_island', 'disvmesh'],

        ['test030_hani_col', 'hanicol'],
        ['test030_hani_row', 'hanirow'],
        ['test030_hani_xt3d', 'hanixt3d'],
        ['test054_xt3d_whirlsA', 'whirlsxt3d'],

        ['test034_nwtp2', 'mfnwt2'],
        ['test014_NWTP3High', 'mfnwt3h'],
        ['test014_NWTP3Low', 'mfnwt3l'],
        ['test013_Zaidel', 'zaidel'],
        ['test016_Keating', 'keating'],

        ['test028_sfr', 'sfr1'],
        ['test045_lake2tr', 'lak2'],
        ['test045_lake4ss', 'lak4'],
        ['test020_NevilleTonkinTransient', 'neville'],
        ['test023_FlowingWell', 'flowing-maw'],
        ['test024_Reilly', 'Reilly-maw'],
        ['test051_uzfp3_wellakmvr_v2', 'advpakmvr'],

        ['test011_mflgr_ex3', 'mflgr3'],
        ['test019_VilhelmsenGC', 'vilhelmsen-gc'],
        ['test019_VilhelmsenGF', 'vilhelmsen-gf'],
        ['test019_VilhelmsenLGR', 'vilhelmsen-lgr'],

        ['test046_periodic_bc', 'periodicbc'],
    ]

    # Create a runall.bat file in examples
    if win_target_os:
        frunallbat = open(os.path.join(expath, 'runall.bat'), 'w')
    else:
        frunallbat = None

    # For each example, copy the necessary files from the development directory
    # into the distribution directory.
    print('Copying examples')
    for i, (exsrc, exdest) in enumerate(examplelist):
        srcpath = os.path.join(exsrcpath, exsrc)

        prefix = 'ex{:02d}-'.format(i + 1)
        destfoldername = prefix + exdest
        dstpath = os.path.join(expath, prefix + exdest)
        print('  {:<35} ===> {:<20}'.format(exsrc, prefix + exdest))

        # Copy all of the mf6 input from srcpath to dstpath
        extrafiles = ['description.txt']
        pymake.setup_mf6(srcpath, dstpath, extrafiles=extrafiles)

        if win_target_os:
            # Create a batch file for running the model
            fname = os.path.join(dstpath, 'run.bat')
            with open(fname, 'w') as f:
                s = '@echo off'
                f.write(s + '\n')
                s = r'..\..\bin\mf6.exe'
                f.write(s + '\n')
                s = 'echo.'
                f.write(s + '\n')
                s = 'echo Run complete.  Press any key to continue.'
                f.write(s + '\n')
                s = 'pause>nul'
                f.write(s + '\n')

            # # Create a batch file for running the model without pausing
            # fname = os.path.join(dstpath, 'run_nopause.bat')
            # with open(fname, 'w') as f:
            #     s = r'..\..\bin\{}.exe'.format(exename)
            #     f.write(s + '\n')

            if frunallbat is not None:
                frunallbat.write('cd ' + destfoldername + '\n')
                frunallbat.write(r'..\..\bin\mf6.exe' + '\n')
                frunallbat.write('cd ..' + '\n\n')
    print('\n')

    if frunallbat is not None:
        frunallbat.write('pause' + '\n')
        frunallbat.close()

    # Clean and then remake latex docs
    clean_latex_files()
    update_mf6io_tex_files(distfolder)
    update_latex_releaseinfo()
    build_latex_docs()

    # docs
    docsrc = os.path.join('..', 'doc')
    doclist = [
               [os.path.join(docsrc, 'ReleaseNotes', 'ReleaseNotes.pdf'), 'release.pdf'],
               [os.path.join(docsrc, 'mf6io', 'mf6io.pdf'), 'mf6io.pdf'],
               [os.path.join(docsrc, 'ConverterGuide', 'converter_mf5to6.pdf'), 'mf5to6.pdf'],
               [os.path.join('..', 'doc', 'zonebudget', 'zonebudget.pdf'), 'zonebudget.pdf'],
               ]

    print('Copying documentation')
    for din, dout in doclist:
        dst = os.path.join(fd['doc'], dout)
        print('  copying {} ===> {}'.format(din, dst))
        shutil.copy(din, dst)
    print('\n')

    print('Downloading published reports for inclusion in distribution')
    for url in ['https://pubs.usgs.gov/tm/06/a57/tm6a57.pdf',
                'https://pubs.usgs.gov/tm/06/a55/tm6a55.pdf',
                'https://pubs.usgs.gov/tm/06/a56/tm6a56.pdf']:
        print('  downloading {}'.format(url))
        download_and_unzip(url, pth=fd['doc'], delete_zip=False)
    print('\n')

    # Prior to zipping, enforce os line endings on all text files
    windows_line_endings = True
    convert_line_endings(distfolder, windows_line_endings)

    # Zip the distribution
    uflag = 'u'
    if win_target_os:
        uflag = ''
    zipname = version + uflag + '.zip'
    if os.path.exists(zipname):
        print('Removing existing file: {}'.format(zipname))
        os.remove(zipname)
    print('Creating zipped file: {}'.format(zipname))
    zipdir(distfolder, zipname)
    print('\n')

    print('Done...')
    print('\n')


"""



# Copy the Window executables
print('Copying mf6 executable')
bin32 = os.path.join('..', 'bin', 'mf6.exe')
shutil.copy(bin32, os.path.join(binpath, 'mf6.exe'))
print('  {} ===> {}'.format(bin32, os.path.join(binpath, 'mf6.exe')))
print('\n')


# Copy the Mac executables
print('Copying mf6.mac')
bin32 = os.path.join('..', 'bin', 'mf6.mac')
shutil.copy(bin32, os.path.join(binpath, 'mf6.mac'))
print('  {} ===> {}'.format(bin32, os.path.join(binpath, 'mf6.mac')))
print('\n')


# Copy the translator executable
print('Copying translator executable')
bin32 = os.path.join('..', 'mf5to6', 'msvs', 'mf5to6', 'Release', 'mf5to6.exe')
shutil.copy(bin32, os.path.join(binpath, 'mf5to6.exe'))
print('  {} ===> {}'.format(bin32, os.path.join(binpath, 'mf5to6.exe')))
print('\n')


# Copy the source folder
s = os.path.join('..', 'src')
shutil.copytree(s, sourcepath, ignore=shutil.ignore_patterns('.DS_Store'))
print('  {} ===> {}'.format(s, sourcepath))
print('\n')


# Copy the Visual Studio solution and project files
flist = [os.path.join('..', 'msvs', 'mf6', 'mf6.sln'),
		 os.path.join('..', 'msvs', 'mf6', 'mf6.vfproj'),
		 ]
print('Copying msvs files')
for d in flist:
	print('  {} ===> {}'.format(d, msvspath)	)
	shutil.copy(d, msvspath)
print('\n')


# Copy the pymake batch and python files
flist = [os.path.join('..', 'pymake', 'makebin.py'),
		 os.path.join('..', 'pymake', 'make_win_gfortran.bat'),
		 os.path.join('..', 'pymake', 'make_win_intel.bat'),
		 ]
print('Copying pymake files')
for d in flist:
	print('  {} ===> {}'.format(d, pymakepath)	)
	shutil.copy(d, pymakepath)
print('\n')


# Copy the documentation
doclist = [os.path.join('..', 'doc', 'GwfModelReport', 'GwfModelReport.01.pdf'),
           os.path.join('..', 'doc', 'ReleaseNotes', 'ReleaseNotes.pdf'),
		   os.path.join('..', 'doc', 'UserGuide', 'userguide.pdf'),
		   os.path.join('..', 'doc', 'ConverterGuide', 'converter_mf5to6.pdf'),
#		   os.path.join('..', 'doc', 'XT3DReport', 'XT3DReport.01.pdf'),
		   ]
print('Copying documentation')
for d in doclist:
	print('  {} ===> {}'.format(d, docpath)	)
	shutil.copy(d, docpath)
print('\n')


# Copy the example problems
exsrcpath = '../examples'
examplelist = [
    ['test021_twri', 'twri'],
    ['AdvGW_tidal', 'tidal'],
    ['test003_gwfs', 'flow2d'],
    ['test004_bcfss', 'bcf2ss'],
    ['test035_fhb', 'fhb'],
    ['test030_hani_col', 'hanicol'],
    ['test030_hani_row', 'hanirow'],
    ['test006_gwf3_gnc', 'mfusgP1u'],
    ['test006_2models', 'mfusgP1Lgr'],
    ['test006_2models_mvr', 'P1LgrMVR'],
    ['test007_751x751', '775x751'],
    ['test028_sfr', 'sfrEx1'],
    ['test045_lake2tr', 'lakEx2'],
    ['test045_lake4ss', 'lakEx4'],
    ['test024_Reilly', 'ReillyMAW'],
    ['test023_FlowingWell', 'FlowingMAW'],
    ['test011_mflgr_ex3', 'mflgrEx3'],
    ['test019_VilhelmsenGC', 'VilhelmsenGC'],
    ['test019_VilhelmsenGF', 'VilhelmsenGF'],
    ['test019_VilhelmsenLGR', 'VilhelmsenLGR'],
    ['test020_NevilleTonkinTransient', 'neville'],
    ['test013_Zaidel', 'zaidel'],
    ['test016_Keating', 'keating'],
    ['test034_nwtp2', 'mfnwtEx2'],
    ['test014_NWTP3High', 'mfnwtEx3H'],
    ['test014_NWTP3Low', 'mfnwtEx3L'],
    ['test050_circle_island', 'islandDISV'],
    ['test008_henry', 'henry'],
    ['test046_periodic_bc', 'periodicbc'],
    ['test031_many_gwf', 'multiGWF'],
    ['test030_hani_xt3d', 'hanixt3d'],
    ]

# Create a runall.bat file in examples
frunallbat = open(os.path.join(expath, 'runall.bat'), 'w')


# For each example, copy the necessary files from the development directory
# into the distribution directory.
print('Copying examples')
for i, (exsrc, exdest) in enumerate(examplelist):
    srcpath = os.path.join(exsrcpath, exsrc)

    prefix = 'ex{:02d}-'.format(i + 1)
    destfoldername = prefix + exdest
    dstpath = os.path.join(expath, prefix + exdest)
    print('  {:<35} ===> {:<20}'.format(exsrc, prefix + exdest))

    # Copy all of the mf6 input from srcpath to dstpath
    extrafiles = ['description.txt']
    mf6pyutil.copy_mf6_input(srcpath, dstpath, extrafiles=extrafiles)

    # Create a batch file for running the model
    fname = os.path.join(dstpath, 'run.bat')
    with open(fname, 'w') as f:
        s = r'..\..\bin\mf6.exe'
        f.write(s + '\n')
        s = 'pause'
        f.write(s + '\n')

    # Create a batch file for running the model without pausing
    fname = os.path.join(dstpath, 'run_nopause.bat')
    with open(fname, 'w') as f:
        s = r'..\..\bin\mf6.exe'
        f.write(s + '\n')

    frunallbat.write('cd ' + destfoldername + '\n')
    frunallbat.write('call run_nopause.bat' + '\n')
    frunallbat.write('cd ..' + '\n\n')
print('\n')

frunallbat.write('pause' + '\n')
frunallbat.close()

# Zip the distribution
zipname = version + '.zip'
if os.path.exists(zipname):
    print('Removing existing file: {}'.format(zipname))
    os.remove(zipname)
print('Creating zipped file: {}'.format(zipname))
zipdir(dest, zipname)
print('\n')

print('Done...')
print('\n')


"""
