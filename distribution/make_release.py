#!/usr/bin/python

"""
make_release.py: Update files in this modflow6 repository according to relase information.

This script is used to update several files in the modflow6 repository, including:

  ../version.txt
  ../doc/version.tex
  ../README.md
  ../DISCLAIMER.md
  ../code.json
  ../src/Utiliteis/version.f90

Command line switches for overriding settings include:

  --version <x.y.z>
  --developMode <idevelop>
  --isApproved
  --releaseCandidate

Information in these files include version number (major.minor.micro), build date, whether or not
the release is a release candidate or an actual release, whether the source code should be compiled
in develop mode or in release mode, and the approval status.

This information is determined using the following logic:

  If the branch name is master or release or the --isApproved argument is specified,
  then it assumes this version is approved, which will result in use of the approved disclaimer.
  Otherwise it is assumed to be provisional.

  If the version is approved (as determined by the previous logic) then the distribution is
  not marked as a release candidate unless it is forced to be so using the --releaseCandidate
  command line argument.

  The version number is read in from ../version.txt, which contains major, minor, and micro version
  numbers.  These numbers will be propagated through the source code, latex files, markdown files,
  etc.  The version numbers can be overridden using the command line argument --version major.minor.macro.

  Develop mode is set to 0 if the distribution is approved or it can be explicitly set using
  the --developMode command line argument.

Once this script is run, these updated files will be used in compilation, latex documents, and
other parts of the repo to mark the overall status.

"""


import datetime
import json
import os
import shutil
import subprocess
import sys
from collections import OrderedDict

# update files and paths so that there are the same number of
# path and file entries in the paths and files list. Enter '.'
# as the path if the file is in the root repository directory
paths = ["../", "../doc", "../", "../", "../", "../src/Utilities"]
files = [
    "version.txt",
    "version.tex",
    "README.md",
    "DISCLAIMER.md",
    "code.json",
    "version.f90",
]

# check that there are the same number of entries in files and paths
if len(paths) != len(files):
    msg = (
        "The number of entries in paths "
        + f"({len(paths)}) must equal "
        + f"the number of entries in files ({len(files)})"
    )
    assert False, msg

prod = "MODFLOW 6"
repo = "MODFLOW-USGS/modflow6.git"

now = datetime.datetime.now()

approved = """Disclaimer
----------

This software has been approved for release by the U.S. Geological Survey
(USGS). Although the software has been subjected to rigorous review, the USGS
reserves the right to update the software as needed pursuant to further analysis
and review. No warranty, expressed or implied, is made by the USGS or the U.S.
Government as to the functionality of the software and related material nor
shall the fact of release constitute any such warranty. Furthermore, the
software is released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
"""

preliminary = """Disclaimer
----------

This software is preliminary or provisional and is subject to revision. It is
being provided to meet the need for timely best science. The software has not
received final approval by the U.S. Geological Survey (USGS). No warranty,
expressed or implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of release
constitute any such warranty. The software is provided on the condition that
neither the USGS nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the software.
"""

approvedfmt = '''  character(len=*), parameter :: FMTDISCLAIMER =                                &
    "(/,                                                                        &
    &'This software has been approved for release by the U.S. Geological ',/,   &
    &'Survey (USGS). Although the software has been subjected to rigorous ',/,  &
    &'review, the USGS reserves the right to update the software as needed ',/, &
    &'pursuant to further analysis and review. No warranty, expressed or ',/,   &
    &'implied, is made by the USGS or the U.S. Government as to the ',/,        &
    &'functionality of the software and related material nor shall the ',/,     &
    &'fact of release constitute any such warranty. Furthermore, the ',/,       &
    &'software is released on condition that neither the USGS nor the U.S. ',/, &
    &'Government shall be held liable for any damages resulting from its ',/,   &
    &'authorized or unauthorized use. Also refer to the USGS Water ',/,         &
    &'Resources Software User Rights Notice for complete use, copyright, ',/,   &
    &'and distribution information.',/)"'''

preliminaryfmt = '''  character(len=*), parameter :: FMTDISCLAIMER =                                &
    "(/,                                                                        &
    &'This software is preliminary or provisional and is subject to ',/,        &
    &'revision. It is being provided to meet the need for timely best ',/,      &
    &'science. The software has not received final approval by the U.S. ',/,    &
    &'Geological Survey (USGS). No warranty, expressed or implied, is made ',/, &
    &'by the USGS or the U.S. Government as to the functionality of the ',/,    &
    &'software and related material nor shall the fact of release ',/,          &
    &'constitute any such warranty. The software is provided on the ',/,        &
    &'condition that neither the USGS nor the U.S. Government shall be held ',/,&
    &'liable for any damages resulting from the authorized or unauthorized ',/, &
    &'use of the software.',/)"'''


def get_disclaimer():
    is_approved = get_is_approved()
    if is_approved:
        disclaimer = approved
    else:
        disclaimer = preliminary
    return is_approved, disclaimer


def get_is_approved():

    is_approved = None

    # override if --isApproved argument was set
    for idx, arg in enumerate(sys.argv):
        if arg == "--isApproved":
            is_approved = True

    if is_approved is None:
        # get current branch
        branch = get_branch()
        if "release" in branch.lower() or "master" in branch.lower():
            is_approved = True
        else:
            is_approved = False

    return is_approved


def get_disclaimerfmt():

    is_approved = get_is_approved()

    if is_approved:
        disclaimer = approvedfmt
    else:
        disclaimer = preliminaryfmt

    return is_approved, disclaimer


def get_branch(verbose=False):
    branch = None

    # determine if branch defined on command line
    for argv in sys.argv:
        if "master" in argv:
            branch = "master"
        elif "develop" in argv.lower():
            branch = "develop"

    if branch is None:
        try:
            # determine current branch
            proc = subprocess.Popen(
                ("git", "branch"),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                cwd=".",
            )
            stdout, stderr = proc.communicate()
            if stdout:
                for line in stdout.decode("utf-8").splitlines():
                    if "* " in line:
                        branch = line.replace("* ", "")
                    if verbose:
                        print(line)
                if verbose:
                    print(f"On Branch: {branch}\n")
            if stderr:
                print(f"Errors:\n{stderr.decode('utf-8')}")

            if branch is not None:
                if "master" in branch or "release" in branch:
                    branch = "master"
                else:
                    branch = "develop"

        except:
            msg = "Could not determine current branch. Is git installed?"
            raise ValueError(msg)

    return branch


def get_version_str(v0, v1, v2):
    version_type = (f"{v0}", f"{v1}", f"{v2}")
    version = ".".join(version_type)
    return version


def get_tag(v0, v1, v2):
    tag_type = (f"{v0}", f"{v1}", f"{v2}")
    tag = ".".join(tag_type)
    return tag


def update_version():
    vmajor = None
    vminor = None
    vmicro = None

    # override if --version argument was set
    for idx, arg in enumerate(sys.argv):
        if arg == "--version":
            t = sys.argv[idx + 1]
            t = t.split(".")
            vmajor = int(t[0])
            vminor = int(t[1])
            vmicro = int(t[2])

    try:
        fpth = os.path.join(paths[0], files[0])
        lines = [line.rstrip("\n") for line in open(fpth, "r")]
        if vmajor is None:
            for line in lines:
                t = line.split()
                if "major =" in line:
                    vmajor = int(t[2])
                elif "minor =" in line:
                    vminor = int(t[2])
                elif "micro =" in line:
                    vmicro = int(t[2])
    except:
        msg = "There was a problem updating the version file"
        raise IOError(msg)

    try:
        # write new version file
        f = open(fpth, "w")
        f.write(
            f"# {prod} version file automatically "
            + f"created using...{os.path.basename(__file__)}\n"
        )
        f.write("# created on..." + f"{now.strftime('%B %d, %Y %H:%M:%S')}\n")
        f.write("\n")
        f.write(f"major = {vmajor}\n")
        f.write(f"minor = {vminor}\n")
        f.write(f"micro = {vmicro}\n")
        f.write("__version__ = '{:d}.{:d}.{:d}'.format(major, minor, micro)\n")
        f.close()
        print("Successfully updated version.py")

        # update version.py in doc directory
        shutil.copyfile(
            os.path.abspath(fpth),
            os.path.join(
                "..", "doc", os.path.basename(fpth.replace(".txt", ".py"))
            ),
        )

        # update latex version file
        version = get_version_str(vmajor, vminor, vmicro)
        version_type = get_version_type(get_branch()).strip()
        if len(version_type) > 0:
            version += f"---{version_type}"
        pth = os.path.join(paths[1], files[1])
        f = open(pth, "w")
        line = "\\newcommand{\\modflowversion}{mf" + f"{version}" + "}"
        f.write(f"{line}\n")
        line = (
            "\\newcommand{\\modflowdate}{"
            + f"{now.strftime('%B %d, %Y')}"
            + "}"
        )
        f.write(f"{line}\n")
        line = (
            "\\newcommand{\\currentmodflowversion}"
            + "{Version \\modflowversion---\\modflowdate}"
        )
        f.write(f"{line}\n")
        f.close()
        print(f"Succesfully updated {files[1]}")
    except:
        msg = "There was a problem updating the version file"
        raise IOError(msg)

    # update version.f90
    update_mf6_version(vmajor, vminor, vmicro)

    # update README.md with new version information
    update_readme_markdown(vmajor, vminor, vmicro)

    # update code.json
    update_codejson(vmajor, vminor, vmicro)


def get_version_type(branch):

    # override if --releaseCandidate argument was set
    version_type = None
    for idx, arg in enumerate(sys.argv):
        if arg == "--releaseCandidate":
            version_type = " release candidate "

    if version_type is None:
        is_approved = get_is_approved()
        version_type = " "
        if not is_approved:
            version_type = " release candidate "

    return version_type


def get_develop_mode(branch):

    # override if --releaseCandidate argument was set
    idevelop = None
    for idx, arg in enumerate(sys.argv):
        if arg == "--developMode":
            ival = sys.argv[idx + 1]
            ival = int(ival)
            idevelop = ival

    if idevelop is None:
        idevelop = 0
        is_approved = get_is_approved()
        if not is_approved:
            idevelop = 1

    return idevelop


def update_mf6_version(vmajor, vminor, vmicro):

    # get branch
    branch = get_branch()

    # create version
    version = get_tag(vmajor, vminor, vmicro)

    # get develop mode
    idevelopmode = get_develop_mode(branch)

    # get version type
    version_type = get_version_type(branch)

    # develop date text
    sdate = now.strftime("%m/%d/%Y")

    # create disclaimer text
    is_approved, disclaimerfmt = get_disclaimerfmt()

    # read version.f90 into memory
    fpth = os.path.join(paths[5], files[5])
    with open(fpth, "r") as file:
        lines = [line.rstrip() for line in file]

    # rewrite version.f90
    skip = False
    f = open(fpth, "w")
    for line in lines:
        # skip all of the disclaimer text
        if skip:
            if ',/)"' in line:
                skip = False
            continue
        elif ":: IDEVELOPMODE =" in line:
            line = (
                "  integer(I4B), parameter :: "
                + f"IDEVELOPMODE = {idevelopmode}"
            )
        elif ":: VERSION =" in line:
            line = (
                "  character(len=40), parameter :: "
                + "VERSION = '{}{}{}'".format(version, version_type, sdate)
            )
        elif ":: FMTDISCLAIMER =" in line:
            line = disclaimerfmt
            skip = True
        f.write(f"{line}\n")
    f.close()

    return


def update_readme_markdown(vmajor, vminor, vmicro):
    # get branch
    branch = get_branch()

    # create version
    version = get_tag(vmajor, vminor, vmicro)

    # create disclaimer text
    is_approved, disclaimer = get_disclaimer()

    if is_approved:
        sb = ""
    else:
        sb = " release candidate"

    # read README.md into memory
    fpth = os.path.join(paths[2], files[2])
    with open(fpth, "r") as file:
        lines = [line.rstrip() for line in file]

    # rewrite README.md
    terminate = False
    f = open(fpth, "w")
    for line in lines:
        if "## Version " in line:
            line = f"### Version {version}"
            if "develop" in branch:
                line += sb
        # This has been commented out as we've generalized this reference.
        # elif "https://doi.org/10.5066/F76Q1VQV" in line:
        #    line = (
        #        "[Langevin, C.D., Hughes, J.D., "
        #        + "Banta, E.R., Provost, A.M., "
        #        + "Niswonger, R.G., and Panday, Sorab, "
        #        + "{}, ".format(now.year)
        #        + "MODFLOW 6 Modular Hydrologic Model "
        #        + "version {}{}: ".format(version, sb)
        #        + "U.S. Geological Survey Software Release, "
        #        + "{}, ".format(now.strftime("%d %B %Y"))
        #        + "https://doi.org/10.5066/F76Q1VQV]"
        #        + "(https://doi.org/10.5066/F76Q1VQV)"
        #    )
        elif "Disclaimer" in line:
            line = disclaimer
            terminate = True
        f.write(f"{line}\n")
        if terminate:
            break
    f.close()

    # write disclaimer markdown file
    fpth = os.path.join(paths[3], files[3])
    f = open(fpth, "w")
    f.write(disclaimer)
    f.close()

    return


def update_codejson(vmajor, vminor, vmicro):
    # define json filename
    json_fname = os.path.join(paths[4], files[4])

    # get branch
    branch = get_branch()

    # create version
    version = get_tag(vmajor, vminor, vmicro)

    is_approved = get_is_approved()

    # load and modify json file
    with open(json_fname, "r") as f:
        data = json.load(f, object_pairs_hook=OrderedDict)

    # modify the json file data
    sdate = now.strftime("%Y-%m-%d")
    data[0]["date"]["metadataLastUpdated"] = sdate
    if is_approved:
        data[0]["version"] = version
        data[0]["status"] = "Production"
    else:
        data[0]["version"] = version
        data[0]["status"] = "Release Candidate"

    # rewrite the json file
    with open(json_fname, "w") as f:
        json.dump(data, f, indent=4)
        f.write("\n")

    return


if __name__ == "__main__":
    update_version()
