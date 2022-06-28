"""
Python code to create a MODFLOW 6 distribution.  This has been used mostly
on Windows and requires that Latex be installed, and Python with the
pymake package.

To make a distribution:
  1.   Install/update pymake, mf6examples, flopy, unix2dos/dos2unix,
       fortran compiler, jupytext, bmipy, xmipy, modflowapi
  2.   Run update_flopy.py in modflow6/autotest
  3.   Put fresh executables (including mf6.exe and libmf6.dll) into
       mf6examples/bin
  4.   Run python scripts in mf6examples/scripts (run process-scripts.py last)
  5.   Create a release branch
  6.   Update version.txt with the correct minor and micro numbers
  7.   Run the make_release.py script, which will create the proper dist name
  8.   Run this mkdist.py script
  9.   Post the distribution zip file
  10.  Commit the release changes, but no need to push
  11.  Merge the release changes into the master branch
  12.  Tag the master branch with the correct version
  13.  Merge master into develop

"""


import os
import shutil
import subprocess
import sys
import zipfile
from contextlib import contextmanager

import pymake
from pymake import download_and_unzip


@contextmanager
def cwd(path):
    oldpwd = os.getcwd()
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
        srchtxt = "modflowversion"
        if srchtxt in line:
            istart = line.rfind("{") + 1
            istop = line.rfind("}")
            if 0 < istart < istop:
                vername = line[istart:istop]
        srchtxt = "modflowdate"
        if srchtxt in line:
            istart = line.rfind("{") + 1
            istop = line.rfind("}")
            if 0 < istart < istop:
                verdate = line[istart:istop]
        if verdate is not None:
            break
    return vername, verdate


def zipdir(dirname, zipname):
    print(f"Zipping directory: {dirname}")
    zipf = zipfile.ZipFile(zipname, "w", zipfile.ZIP_DEFLATED)
    for root, dirs, files in os.walk(dirname):
        for file in files:
            if ".DS_Store" not in file:
                fname = os.path.join(root, file)
                print("  Adding to zip: ==> ", fname)
                zipf.write(fname, arcname=fname)
    zipf.close()
    print("\n")
    return


def setup(name, destpath, version, subdirs):
    """
    Setup the folder structure, and return a dictionary of subfolder name
    and the full path in destpath.

    """
    print(2 * "\n")
    print(f"Setting up {name} distribution: {version}")
    print("\n")

    dest = os.path.join(destpath, version)
    if os.path.exists(dest):
        # Raise Exception('Destination path exists.  Kill it first.')
        print(f"Clobbering destination directory: {dest}")
        print("\n")
        shutil.rmtree(dest)
    os.mkdir(dest)

    print("Creating subdirectories")
    folderdict = {}
    for sd in subdirs:
        fullpath = os.path.join(dest, sd)
        print(f"  creating ==> {fullpath}")
        os.mkdir(fullpath)
        folderdict[sd] = fullpath
    print("\n")

    return folderdict


def copytree(src, dst, symlinks=False, ignore=None):
    """
    Copy a folder from src to dst.  If dst does not exist, then create it.

    """
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            print(f"  copying {s} ===> {d}")
            shutil.copytree(s, d, symlinks, ignore)
        else:
            print(f"  copying {s} ===> {d}")
            shutil.copy2(s, d)
    return


def convert_line_endings(folder, windows=True):
    """
    Convert all of the line endings to windows or unix

    """
    # Prior to zipping, enforce os line endings on all text files
    print("Converting line endings...")
    platform = sys.platform
    cmd = None
    if platform.lower() == "darwin":
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
    print("\n")
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
        srchtxt = "character(len=40), parameter :: VERSION"
        if srchtxt in line:
            newline = f"{srchtxt} = '{version}'"
            found1 = True
        srchtxt = "integer(I4B), parameter :: IDEVELOPMODE"
        if srchtxt in line:
            newline = f"{srchtxt} = {0}"
            found2 = True
        newlines.append(newline)
    if not found1 or not found2:
        raise Exception(
            "could not replace version or developmode in source code"
        )
    with open(fname, "w") as f:
        for line in newlines:
            f.write(line.strip() + "\n")
    return


def make_zonebudget(srcpath, destpath, win_target_os, exepath):
    """
    Add zone budget to the distribution

    srcpath should be '../utils/zonebudget'
    destpath should be 'utils'
    sourcepath


    """

    # setup the folder structure
    name = "zonebudget"
    version = "zonebudget"
    subdirs = ["src", "make", "msvs"]
    fd = setup(name, destpath, version, subdirs)

    # copy source folder
    sourcepath = os.path.join(srcpath, "src")
    copytree(sourcepath, fd["src"], ignore=shutil.ignore_patterns(".DS_Store"))

    # Create makefile in the utils/zonebudget/pymake folder
    print("Creating zonebudget makefile")
    with cwd(os.path.join(srcpath, "pymake")):
        pymake.main(
            os.path.join("..", "src"),
            "zbud6",
            "gfortran",
            "gcc",
            makeclean=True,
            dryrun=True,
            include_subdirs=True,
            makefile=True,
            extrafiles="extrafiles.txt",
        )
        os.path.isfile("makefile")
        os.path.isfile("makedefaults")

    # Copy makefile to utils/zonebudget/make folder
    shutil.copyfile(
        os.path.join(srcpath, "pymake", "makefile"),
        os.path.join(srcpath, "make", "makefile"),
    )
    shutil.copyfile(
        os.path.join(srcpath, "pymake", "makedefaults"),
        os.path.join(srcpath, "make", "makedefaults"),
    )

    # Copy makefile to distribution/xxx/utils/zonebudget/make folder
    shutil.copyfile(
        os.path.join(srcpath, "pymake", "makefile"),
        os.path.join(fd["make"], "makefile"),
    )
    shutil.copyfile(
        os.path.join(srcpath, "pymake", "makedefaults"),
        os.path.join(fd["make"], "makedefaults"),
    )

    # Remove the makefile from the pymake folder
    os.remove(os.path.join(srcpath, "pymake", "makefile"))
    os.remove(os.path.join(srcpath, "pymake", "makedefaults"))

    # Copy the Visual Studio project file
    flist = [os.path.join(srcpath, "msvs", "zonebudget.vfproj")]
    print("Copying zonebudget msvs files")
    for d in flist:
        print(f"  {d} ===> {fd['msvs']}")
        shutil.copy(d, fd["msvs"])
    print("\n")

    # build the executable
    exename = "zbud6"
    target = os.path.join(exepath, exename)
    if win_target_os:
        fc = "ifort"
        cc = "cl"
        exename += ".exe"
    else:
        fc = "gfortran"
        cc = "gcc"
    extrafiles = os.path.join(srcpath, "pymake", "extrafiles.txt")
    pymake.main(
        fd["src"],
        target,
        fc,
        cc,
        makeclean=True,
        include_subdirs=True,
        extrafiles=extrafiles,
    )
    if win_target_os:
        target += ".exe"
    if not os.path.isfile(target):
        raise Exception(f"Did not build target: {target}")

    return


def make_mf5to6(srcpath, destpath, win_target_os, exepath):
    """
    Add mf5to6 to the distribution

    srcpath should be '../utils/mf5to6'
    destpath should be 'utils'
    sourcepath


    """

    # setup the folder structure
    name = "mf5to6"
    version = "mf5to6"
    subdirs = ["src", "make", "msvs"]
    fd = setup(name, destpath, version, subdirs)

    # copy source folder
    sourcepath = os.path.join(srcpath, "src")
    copytree(sourcepath, fd["src"], ignore=shutil.ignore_patterns(".DS_Store"))

    # Create makefile in the utils/mf5to6/pymake folder
    print("Creating mf5to6 makefile")
    with cwd(os.path.join(srcpath, "pymake")):
        pymake.main(
            os.path.join("..", "src"),
            name,
            "gfortran",
            "gcc",
            makeclean=True,
            dryrun=True,
            include_subdirs=True,
            makefile=True,
            extrafiles="extrafiles.txt",
        )
        os.path.isfile("makefile")
        os.path.isfile("makedefaults")

    # Copy makefile to utils/mf5to6/make folder
    print("Copying mf5to6 makefile")
    for fname in ["makefile", "makedefaults"]:
        fpath = os.path.join(srcpath, "pymake", fname)
        d = os.path.join(srcpath, "make", fname)
        print(f"  {fpath} ===> {d}")
        shutil.copyfile(fpath, d)

    # Copy makefile to distribution/xxx/utils/mf5to6/make folder
    for fname in ["makefile", "makedefaults"]:
        fpath = os.path.join(srcpath, "pymake", fname)
        d = os.path.join(fd["make"], fname)
        print(f"  {fpath} ===> {d}")
        shutil.copyfile(fpath, d)

    # Remove makefile and makedefaults from the pymake folder
    for fname in ["makefile", "makedefaults"]:
        fpath = os.path.join(srcpath, "pymake", fname)
        os.remove(fpath)

    # Copy the Visual Studio project file
    flist = [os.path.join(srcpath, "msvs", "mf5to6.vfproj")]
    print("Copying mf5to6 msvs files")
    for d in flist:
        print(f"  {d} ===> {fd['msvs']}")
        shutil.copy(d, fd["msvs"])
    print("\n")

    # build the executable
    exename = "mf5to6"
    target = os.path.join(exepath, exename)
    if win_target_os:
        fc = "ifort"
        cc = "cl"
        exename += ".exe"
    else:
        fc = "gfortran"
        cc = "gcc"
    extrafiles = os.path.join(srcpath, "pymake", "extrafiles.txt")
    pymake.main(
        fd["src"],
        target,
        fc,
        cc,
        makeclean=True,
        include_subdirs=True,
        extrafiles=extrafiles,
    )
    if win_target_os:
        target += ".exe"
    if not os.path.isfile(target):
        raise Exception(f"Did not build target: {target}")

    return


def delete_files(files, pth, allow_failure=False):
    for file in files:
        fpth = os.path.join(pth, file)
        try:
            print(f"removing...{file}")
            os.remove(fpth)
        except:
            print(f"could not remove...{file}")
            if not allow_failure:
                return False
    return True


def run_command(argv, pth, timeout=None):
    with subprocess.Popen(
        argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, cwd=pth
    ) as process:
        try:
            output, unused_err = process.communicate(timeout=timeout)
            buff = output.decode("utf-8")
            ierr = process.returncode
        except subprocess.TimeoutExpired:
            process.kill()
            output, unused_err = process.communicate()
            buff = output.decode("utf-8")
            ierr = 100
        except:
            output, unused_err = process.communicate()
            buff = output.decode("utf-8")
            ierr = 101

    return buff, ierr


def clean_latex_files():

    print("Cleaning latex files")
    exts = ["pdf", "aux", "bbl", "idx", "lof", "out", "toc"]
    pth = os.path.join("..", "doc", "mf6io")
    files = [f"mf6io.{e}" for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + ".pdf")

    pth = os.path.join("..", "doc", "ReleaseNotes")
    files = [f"ReleaseNotes.{e}" for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + ".pdf")

    pth = os.path.join("..", "doc", "zonebudget")
    files = [f"zonebudget.{e}" for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + ".pdf")

    pth = os.path.join("..", "doc", "ConverterGuide")
    files = [f"converter_mf5to6.{e}" for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + ".pdf")

    pth = os.path.join("..", "..", "modflow6-docs.git", "mf6suptechinfo")
    files = [f"mf6suptechinfo.{e}" for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + ".pdf")

    pth = os.path.join("..", "..", "modflow6-examples.git", "doc")
    files = [f"mf6examples.{e}" for e in exts]
    delete_files(files, pth, allow_failure=True)
    assert not os.path.isfile(pth + ".pdf")

    return


def rebuild_tex_from_dfn():

    npth = os.path.join("..", "doc", "mf6io", "mf6ivar")
    pth = "./"

    with cwd(npth):

        # get list of TeX files
        files = [
            f
            for f in os.listdir("tex")
            if os.path.isfile(os.path.join("tex", f))
        ]
        for f in files:
            fpth = os.path.join("tex", f)
            os.remove(fpth)

        # run python
        argv = ["python", "mf6ivar.py"]
        buff, ierr = run_command(argv, pth)
        msg = f"\nERROR {ierr}: could not run {argv[0]} with {argv[1]}"
        assert ierr == 0, buff + msg

        # get list for dfn files
        dfnfiles = [
            os.path.splitext(f)[0]
            for f in os.listdir("dfn")
            if os.path.isfile(os.path.join("dfn", f))
            and "dfn" in os.path.splitext(f)[1]
        ]
        texfiles = [
            os.path.splitext(f)[0]
            for f in os.listdir("tex")
            if os.path.isfile(os.path.join("tex", f))
            and "tex" in os.path.splitext(f)[1]
        ]
        missing = ""
        icnt = 0
        for f in dfnfiles:
            if "common" in f:
                continue
            fpth = f"{f}-desc"
            if fpth not in texfiles:
                icnt += 1
                missing += f"  {icnt:3d} {fpth}.tex\n"
        msg = (
            "\n{} TeX file(s) are missing. ".format(icnt)
            + f"Missing files:\n{missing}"
        )
        assert icnt == 0, msg

    return


def update_mf6io_tex_files(distfolder, mf6pth, expth=None):

    texpth = "../doc/mf6io"
    fname1 = os.path.join(texpth, "mf6output.tex")
    fname2 = os.path.join(texpth, "mf6noname.tex")
    fname3 = os.path.join(texpth, "mf6switches.tex")
    local = False
    if expth is None:
        local = True
        expth = os.path.join(distfolder, "examples", "ex-gwf-twri01")
    expth = os.path.abspath(expth)

    assert os.path.isfile(mf6pth), f"{mf6pth} does not exist"
    assert os.path.isdir(expth), f"{expth} does not exist"

    # run an example model
    if local:
        if os.path.isdir("./temp"):
            shutil.rmtree("./temp")
        shutil.copytree(expth, "./temp")
    cmd = [os.path.abspath(mf6pth)]
    if local:
        simpth = "./temp"
    else:
        simpth = expth
    buff, ierr = run_command(cmd, simpth)
    lines = buff.split("\r\n")
    with open(fname1, "w") as f:
        f.write("{\\small\n")
        f.write("\\begin{lstlisting}[style=modeloutput]\n")
        for line in lines:
            f.write(line.rstrip() + "\n")
        f.write("\\end{lstlisting}\n")
        f.write("}\n")

    # run model without a namefile present
    if os.path.isdir("./temp"):
        shutil.rmtree("./temp")
    os.mkdir("./temp")
    cmd = [os.path.abspath(mf6pth)]
    buff, ierr = run_command(cmd, "./temp")
    lines = buff.split("\r\n")
    with open(fname2, "w") as f:
        f.write("{\\small\n")
        f.write("\\begin{lstlisting}[style=modeloutput]\n")
        for line in lines:
            f.write(line.rstrip() + "\n")
        f.write("\\end{lstlisting}\n")
        f.write("}\n")

    # run mf6 command with -h to show help
    cmd = [os.path.abspath(mf6pth), "-h"]
    buff, ierr = run_command(cmd, "./temp")
    lines = buff.split("\r\n")
    with open(fname3, "w") as f:
        f.write("{\\small\n")
        f.write("\\begin{lstlisting}[style=modeloutput]\n")
        for line in lines:
            f.write(line.rstrip() + "\n")
        f.write("\\end{lstlisting}\n")
        f.write("}\n")

    # clean up
    if os.path.isdir("./temp"):
        shutil.rmtree("./temp")

    return


def build_latex_docs():
    print("Building latex files")
    pth1 = os.path.join("..", "doc")
    pth2 = os.path.join("..", "..", "modflow6-docs.git")
    pth3 = os.path.join("..", "..", "modflow6-examples.git")
    doclist = [
        (pth1, "mf6io", "mf6io.tex"),
        (pth1, "ReleaseNotes", "ReleaseNotes.tex"),
        (pth1, "zonebudget", "zonebudget.tex"),
        (pth1, "ConverterGuide", "converter_mf5to6.tex"),
        (pth2, "mf6suptechinfo", "mf6suptechinfo.tex"),
        (pth3, "doc", "mf6examples.tex"),
    ]

    # copy version.tex from doc to modflow6-docs
    shutil.copy(os.path.join(pth1, "version.tex"), pth2)

    for p, d, t in doclist:
        print(f"Building latex document: {t}")
        dirname = os.path.join(p, d)
        with cwd(dirname):

            pdflatexcmd = [
                "pdflatex",
                "-interaction=nonstopmode",
                "-halt-on-error",
                t,
            ]

            print("  Pass 1/4...")
            cmd = pdflatexcmd
            buff, ierr = run_command(cmd, "./")
            msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
            assert ierr == 0, buff + msg

            cmd = ["bibtex", os.path.splitext(t)[0] + ".aux"]
            print("  Pass 2/4...")
            buff, ierr = run_command(cmd, "./")
            msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
            assert ierr == 0, buff + msg

            print("  Pass 3/4...")
            cmd = pdflatexcmd
            buff, ierr = run_command(cmd, "./")
            msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
            assert ierr == 0, buff + msg

            print("  Pass 4/4...")
            cmd = pdflatexcmd
            buff, ierr = run_command(cmd, "./")
            msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
            assert ierr == 0, buff + msg

            fname = os.path.splitext(t)[0] + ".pdf"
            assert os.path.isfile(fname), "Could not find " + fname

    return


def update_latex_releaseinfo(examples_folder):

    pth = os.path.join("..", "doc", "ReleaseNotes")
    files = ["folder_struct.tex"]
    delete_files(files, pth, allow_failure=True)

    cmd = ["python", "mk_folder_struct.py"]
    buff, ierr = run_command(cmd, pth)
    msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
    assert ierr == 0, buff + msg

    cmd = ["python", "mk_runtimecomp.py"]
    buff, ierr = run_command(cmd, pth)
    msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
    assert ierr == 0, buff + msg

    for f in files:
        assert os.path.isfile(os.path.join(pth, f)), (
            "File does not exist: " + f
        )

    return


def setup_examples(examples_repo, exdestpath, mf6path):

    # trap
    assert os.path.isdir(examples_repo)
    assert os.path.isdir(exdestpath)

    # next create all examples, but don't run them
    scripts_folder = os.path.join(examples_repo, "scripts")
    scripts_folder = os.path.abspath(scripts_folder)
    exclude_list = ["ex-gwf-capture.py"]
    scripts = [
        fname
        for fname in os.listdir(scripts_folder)
        if fname.endswith(".py")
        and fname.startswith("ex-")
        and fname not in exclude_list
    ]
    for script in scripts:
        dest = os.path.abspath(exdestpath)
        argv = [
            "python",
            script,
            "--no_run",
            "--no_plot",
            "--destination",
            dest,
        ]  # no run no plot
        print(f"running {argv} in {scripts_folder}")
        run_command(argv, scripts_folder)

    # create list of folders with mfsim.nam
    simulation_folders = []
    for root, dirs, files in os.walk(exdestpath):
        for d in dirs:
            dwpath = os.path.join(root, d)
            if "mfsim.nam" in os.listdir(dwpath):
                simulation_folders.append(dwpath)
    simulation_folders = sorted(simulation_folders)

    # go through each simulation folder and add a run.bat file
    for dwpath in simulation_folders:
        fname = os.path.join(dwpath, "run.bat")
        print(f"Adding {fname}")
        with open(fname, "w") as f:
            f.write("@echo off" + "\n")
            runbatloc = os.path.relpath(mf6path, start=dwpath)
            f.write(runbatloc + "\n")
            f.write("echo." + "\n")
            f.write("echo Run complete.  Press any key to continue" + "\n")
            f.write("pause>nul" + "\n")

    # add runall.bat, which runs all examples
    fname = os.path.join(exdestpath, "runall.bat")
    with open(fname, "w") as f:
        for dwpath in simulation_folders:
            d = os.path.relpath(dwpath, start=exdestpath)
            s = f"cd {d}"
            f.write(s + "\n")
            runbatloc = os.path.relpath(mf6path, start=dwpath)
            f.write(runbatloc + "\n")
            d = os.path.relpath(exdestpath, start=dwpath)
            s = f"cd {d}"
            f.write(s + "\n")
            s = ""
            f.write(s + "\n")
        f.write("pause" + "\n")

    return


if __name__ == "__main__":

    # setup paths and folder structure
    win_target_os = False
    if sys.platform.lower() == "win32":
        win_target_os = True

    name = "MODFLOW 6"
    exename = "mf6"
    destpath = "."
    versiontexname = os.path.join("..", "doc", "version.tex")
    version, versiondate = get_distribution_info(versiontexname)
    distfolder = os.path.join(destpath, version)
    subdirs = [
        "bin",
        "doc",
        "examples",
        "src",
        "srcbmi",
        "msvs",
        "make",
        "utils",
    ]
    fd = setup(name, destpath, version, subdirs)

    # Copy the Visual Studio solution and project files
    flist = [
        os.path.join("..", "msvs", "mf6.sln"),
        os.path.join("..", "msvs", "mf6.vfproj"),
        os.path.join("..", "msvs", "mf6core.vfproj"),
        os.path.join("..", "msvs", "mf6bmi.sln"),
        os.path.join("..", "msvs", "mf6bmi.vfproj"),
    ]
    print("Copying msvs files")
    for d in flist:
        print(f"  {d} ===> {fd['msvs']}")
        shutil.copy(d, fd["msvs"])
    print("\n")

    # copy source folder
    copytree(
        os.path.join("..", "src"),
        fd["src"],
        ignore=shutil.ignore_patterns(".DS_Store"),
    )

    # copy srcbmi folder
    copytree(
        os.path.join("..", "srcbmi"),
        fd["srcbmi"],
        ignore=shutil.ignore_patterns(".DS_Store"),
    )

    # Remove existing makefile and makedefaults
    print("Creating makefile")
    makedir = os.path.join("..", "make")
    for fname in ["makefile", "makedefaults"]:
        fpath = os.path.join(makedir, fname)
        if os.path.isfile(fpath):
            os.remove(fpath)

    # Create makefile in the make folder
    with cwd(makedir):
        pymake.main(
            os.path.join("..", "src"),
            "mf6",
            "gfortran",
            "gcc",
            makeclean=True,
            dryrun=True,
            include_subdirs=True,
            makefile=True,
            extrafiles=None,
        )

    # Copy makefile to the distribution
    for fname in ["makefile", "makedefaults"]:
        fpath = os.path.join(makedir, fname)
        print(f"  {fpath} ===> {fd['make']}")
        shutil.copy(fpath, fd["make"])

    # build MODFLOW 6 executable
    srcdir = fd["src"]
    target = os.path.join(fd["bin"], exename)
    if win_target_os:
        fc = "ifort"
        cc = "cl"
    else:
        fc = "gfortran"
        cc = "gcc"
    pymake.main(srcdir, target, fc, cc, makeclean=True, include_subdirs=True)
    if win_target_os:
        target += ".exe"
    if not os.path.isfile(target):
        raise Exception(f"Did not build target: {target}")

    # setup zone budget
    make_zonebudget(
        os.path.join("..", "utils", "zonebudget"),
        fd["utils"],
        win_target_os,
        fd["bin"],
    )

    # setup mf5to6
    make_mf5to6(
        os.path.join("..", "utils", "mf5to6"),
        fd["utils"],
        win_target_os,
        fd["bin"],
    )

    # setup the examples
    exdstpath = fd["examples"]
    examples_repo = os.path.join("..", "..", "modflow6-examples.git")
    setup_examples(examples_repo, exdstpath, target)

    # run the comparison tests so the run time comparison table can be
    # created for the release notes
    cmd = ["python", "evaluate_run_times.py"]
    pth = "."
    buff, ierr = run_command(cmd, pth)
    msg = f"\nERROR {ierr}: could not run {cmd[0]} on {cmd[1]}"
    assert ierr == 0, buff + msg

    # Clean and then remake latex docs
    clean_latex_files()
    rebuild_tex_from_dfn()
    update_mf6io_tex_files(distfolder, target)
    update_latex_releaseinfo(fd["examples"])
    build_latex_docs()

    # docs
    docsrc = os.path.join("..", "doc")
    doclist = [
        [
            os.path.join(docsrc, "ReleaseNotes", "ReleaseNotes.pdf"),
            "release.pdf",
        ],
        [os.path.join(docsrc, "mf6io", "mf6io.pdf"), "mf6io.pdf"],
        [
            os.path.join(docsrc, "ConverterGuide", "converter_mf5to6.pdf"),
            "mf5to6.pdf",
        ],
        [
            os.path.join("..", "doc", "zonebudget", "zonebudget.pdf"),
            "zonebudget.pdf",
        ],
        [
            os.path.join(
                "..",
                "..",
                "modflow6-docs.git",
                "mf6suptechinfo",
                "mf6suptechinfo.pdf",
            ),
            "mf6suptechinfo.pdf",
        ],
        [
            os.path.join(
                "..", "..", "modflow6-examples.git", "doc", "mf6examples.pdf"
            ),
            "mf6examples.pdf",
        ],
    ]

    print("Copying documentation")
    for din, dout in doclist:
        dst = os.path.join(fd["doc"], dout)
        print(f"  copying {din} ===> {dst}")
        shutil.copy(din, dst)
    print("\n")

    print("Downloading published reports for inclusion in distribution")
    for url in [
        "https://pubs.usgs.gov/tm/06/a57/tm6a57.pdf",
        "https://pubs.usgs.gov/tm/06/a55/tm6a55.pdf",
        "https://pubs.usgs.gov/tm/06/a56/tm6a56.pdf",
    ]:
        print(f"  downloading {url}")
        download_and_unzip(url, pth=fd["doc"], delete_zip=False, verify=False)
    print("\n")

    # Prior to zipping, enforce os line endings on all text files
    windows_line_endings = True
    convert_line_endings(distfolder, windows_line_endings)

    # Zip the distribution
    uflag = "u"
    if win_target_os:
        uflag = ""
    zipname = version + uflag + ".zip"
    if os.path.exists(zipname):
        print(f"Removing existing file: {zipname}")
        os.remove(zipname)
    print(f"Creating zipped file: {zipname}")
    zipdir(distfolder, zipname)
    print("\n")

    print("Done...")
    print("\n")
