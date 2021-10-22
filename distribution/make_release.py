#!/usr/bin/python

from __future__ import print_function
import subprocess
import os
import sys
import shutil
import datetime
import json
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
        + "({}) must equal ".format(len(paths))
        + "the number of entries in files ({})".format(len(files))
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
    # get current branch
    branch = get_branch()

    if "release" in branch.lower() or "master" in branch.lower():
        disclaimer = approved
        is_approved = True
    else:
        disclaimer = preliminary
        is_approved = False

    return is_approved, disclaimer


def get_disclaimerfmt():
    # get current branch
    branch = get_branch()

    if "release" in branch.lower() or "master" in branch.lower():
        disclaimer = approvedfmt
        is_approved = True
    else:
        disclaimer = preliminaryfmt
        is_approved = False

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
                    print("On Branch: {}\n".format(branch))
            if stderr:
                print("Errors:\n{}".format(stderr.decode("utf-8")))

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
    version_type = ("{}".format(v0), "{}".format(v1), "{}".format(v2))
    version = ".".join(version_type)
    return version


def get_tag(v0, v1, v2):
    tag_type = ("{}".format(v0), "{}".format(v1), "{}".format(v2))
    tag = ".".join(tag_type)
    return tag


def update_version():
    branch = get_branch(verbose=True)
    try:
        fpth = os.path.join(paths[0], files[0])

        vmajor = 0
        vminor = 0
        vmicro = 0
        lines = [line.rstrip("\n") for line in open(fpth, "r")]
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
            "# {} version file automatically ".format(prod)
            + "created using...{}\n".format(os.path.basename(__file__))
        )
        f.write(
            "# created on..."
            + "{}\n".format(now.strftime("%B %d, %Y %H:%M:%S"))
        )
        f.write("\n")
        f.write("major = {}\n".format(vmajor))
        f.write("minor = {}\n".format(vminor))
        f.write("micro = {}\n".format(vmicro))
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
            version += "---{}".format(version_type)
        pth = os.path.join(paths[1], files[1])
        f = open(pth, "w")
        line = "\\newcommand{\\modflowversion}{mf" + "{}".format(version) + "}"
        f.write("{}\n".format(line))
        line = (
            "\\newcommand{\\modflowdate}{"
            + "{}".format(now.strftime("%B %d, %Y"))
            + "}"
        )
        f.write("{}\n".format(line))
        line = (
            "\\newcommand{\\currentmodflowversion}"
            + "{Version \\modflowversion---\\modflowdate}"
        )
        f.write("{}\n".format(line))
        f.close()
        print("Succesfully updated {}".format(files[1]))
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
    version_type = " "
    if branch is not None:
        if "release" not in branch.lower() and "master" not in branch.lower():
            version_type = " release candidate "
    return version_type


def update_mf6_version(vmajor, vminor, vmicro):
    branch = get_branch()

    # create version
    version = get_tag(vmajor, vminor, vmicro)
    idevelopmode = 0
    if "release" not in branch.lower() and "master" not in branch.lower():
        idevelopmode = 1

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
                + "IDEVELOPMODE = {}".format(idevelopmode)
            )
        elif ":: VERSION =" in line:
            line = (
                "  character(len=40), parameter :: "
                + "VERSION = '{}{}{}'".format(version, version_type, sdate)
            )
        elif ":: FMTDISCLAIMER =" in line:
            line = disclaimerfmt
            skip = True
        f.write("{}\n".format(line))
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
            line = "### Version {}".format(version)
            if "develop" in branch:
                line += sb
        elif "https://doi.org/10.5066/F76Q1VQV" in line:
            line = (
                "[Langevin, C.D., Hughes, J.D., "
                + "Banta, E.R., Provost, A.M., "
                + "Niswonger, R.G., and Panday, Sorab, "
                + "{}, ".format(now.year)
                + "MODFLOW 6 Modular Hydrologic Model "
                + "version {}{}: ".format(version, sb)
                + "U.S. Geological Survey Software Release, "
                + "{}, ".format(now.strftime("%d %B %Y"))
                + "https://doi.org/10.5066/F76Q1VQV]"
                + "(https://doi.org/10.5066/F76Q1VQV)"
            )
        elif "Disclaimer" in line:
            line = disclaimer
            terminate = True
        f.write("{}\n".format(line))
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

    # load and modify json file
    with open(json_fname, "r") as f:
        data = json.load(f, object_pairs_hook=OrderedDict)

    # modify the json file data
    sdate = now.strftime("%Y-%m-%d")
    data[0]["date"]["metadataLastUpdated"] = sdate
    if "release" in branch.lower() or "master" in branch.lower():
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
