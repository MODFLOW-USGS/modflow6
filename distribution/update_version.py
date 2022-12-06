#!/usr/bin/python

"""
Update files in this modflow6 repository according to release information.

This script is used to update several files in the modflow6 repository, including:

  ../version.txt
  ../doc/version.tex
  ../README.md
  ../DISCLAIMER.md
  ../code.json
  ../src/Utilities/version.f90

Information in these files include version number (major.minor.patch), build date, whether or not
the release is a release candidate or an actual release, whether the source code should be compiled
in develop mode or in release mode, and the approval status.

The version number is read in from ../../version.txt, which contains major, minor, and patch version
numbers.  These numbers will be propagated through the source code, latex files, markdown files,
etc.  The version numbers can be overridden using the command line argument --version major.minor.macro.

Develop mode is set to 0 if the distribution is approved.

Once this script is run, these updated files will be used in compilation, latex documents, and
other parts of the repo to mark the overall status.

"""
import argparse
import json
import os
import shutil
import textwrap
from collections import OrderedDict
from datetime import datetime
from enum import Enum
from os import PathLike
from pathlib import Path
from typing import NamedTuple, Optional

import pytest
from filelock import FileLock

from utils import get_modified_time

project_name = "MODFLOW 6"
project_root_path = Path(__file__).parent.parent
version_file_path = project_root_path / "version.txt"
touched_file_paths = [
    version_file_path,
    project_root_path / "doc" / "version.tex",
    project_root_path / "doc" / "version.py",
    project_root_path / "README.md",
    project_root_path / "DISCLAIMER.md",
    project_root_path / "code.json",
    project_root_path / "src" / "Utilities" / "version.f90",
]


class Version(NamedTuple):
    """Semantic version number, not including extensions (e.g., 'Release Candidate')"""

    major: int = 0
    minor: int = 0
    patch: int = 0

    def __repr__(self):
        return f"{self.major}.{self.minor}.{self.patch}"

    @classmethod
    def from_string(cls, version: str) -> "Version":
        t = version.split(".")

        vmajor = int(t[0])
        vminor = int(t[1])
        vpatch = int(t[2])

        return cls(major=vmajor, minor=vminor, patch=vpatch)

    @classmethod
    def from_file(cls, path: PathLike) -> "Version":
        path = Path(path).expanduser().absolute()
        lines = [line.rstrip("\n") for line in open(Path(path), "r")]

        vmajor = vminor = vpatch = None
        for line in lines:
            t = line.split()
            if "major =" in line:
                vmajor = int(t[2])
            elif "minor =" in line:
                vminor = int(t[2])
            elif "micro =" in line:
                vpatch = int(t[2])

        msg = "version string must follow semantic version format: major.minor.patch"
        assert vmajor is not None, f"Missing major number, {msg}"
        assert vminor is not None, f"Missing minor number, {msg}"
        assert vpatch is not None, f"Missing patch number, {msg}"

        return cls(major=vmajor, minor=vminor, patch=vpatch)


class ReleaseType(Enum):
    CANDIDATE = "Release Candidate"
    APPROVED = "Release"



_approved_fmtdisclaimer = '''  character(len=*), parameter :: FMTDISCLAIMER = &
    "(/,&
    &'This software has been approved for release by the U.S. Geological ',/,&
    &'Survey (USGS). Although the software has been subjected to rigorous ',/,&
    &'review, the USGS reserves the right to update the software as needed ',/,&
    &'pursuant to further analysis and review. No warranty, expressed or ',/,&
    &'implied, is made by the USGS or the U.S. Government as to the ',/,&
    &'functionality of the software and related material nor shall the ',/,&
    &'fact of release constitute any such warranty. Furthermore, the ',/,&
    &'software is released on condition that neither the USGS nor the U.S. ',/,&
    &'Government shall be held liable for any damages resulting from its ',/,&
    &'authorized or unauthorized use. Also refer to the USGS Water ',/,&
    &'Resources Software User Rights Notice for complete use, copyright, ',/,&
    &'and distribution information.',/)"'''

_preliminary_fmtdisclaimer = '''  character(len=*), parameter :: FMTDISCLAIMER = &
    "(/,&
    &'This software is preliminary or provisional and is subject to ',/,&
    &'revision. It is being provided to meet the need for timely best ',/,&
    &'science. The software has not received final approval by the U.S. ',/,&
    &'Geological Survey (USGS). No warranty, expressed or implied, is made ',/,&
    &'by the USGS or the U.S. Government as to the functionality of the ',/,&
    &'software and related material nor shall the fact of release ',/,&
    &'constitute any such warranty. The software is provided on the ',/,&
    &'condition that neither the USGS nor the U.S. Government shall be held ',/,&
    &'liable for any damages resulting from the authorized or unauthorized ',/,&
    &'use of the software.',/)"'''

_approved_disclaimer = """Disclaimer
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

_preliminary_disclaimer = """Disclaimer
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


def get_disclaimer(release_type: ReleaseType, formatted: bool = False) -> str:
    approved = _approved_fmtdisclaimer if formatted else _approved_disclaimer
    preliminary = _preliminary_fmtdisclaimer if formatted else _preliminary_disclaimer
    return approved if release_type == ReleaseType.APPROVED else preliminary


def log_update(path, release_type: ReleaseType, version: Version):
    print(f"Updated {path} with version {version}" + (f" {release_type.value}" if release_type != ReleaseType.APPROVED else ""))


def update_version_txt_and_py(
    release_type: ReleaseType, timestamp: datetime, version: Version
):
    with open(version_file_path, "w") as f:
        f.write(
            f"# {project_name} version file automatically "
            + f"created using...{os.path.basename(__file__)}\n"
        )
        f.write("# created on..." + f"{timestamp.strftime('%B %d, %Y %H:%M:%S')}\n")
        f.write("\n")
        f.write(f"major = {version.major}\n")
        f.write(f"minor = {version.minor}\n")
        f.write(f"micro = {version.patch}\n")
        f.write("__version__ = '{:d}.{:d}.{:d}'.format(major, minor, micro)\n")
        f.close()
    log_update(version_file_path, release_type, version)

    py_path = project_root_path / "doc" / version_file_path.name.replace(".txt", ".py")
    shutil.copyfile(version_file_path, py_path)
    log_update(py_path, release_type, version)


def update_version_tex(
    release_type: ReleaseType, timestamp: datetime, version: Version
):
    path = project_root_path / "doc" / "version.tex"

    version_str = str(version)
    if release_type != ReleaseType.APPROVED:
        version_str += "rc"

    with open(path, "w") as f:
        line = "\\newcommand{\\modflowversion}{mf" + f"{version_str}" + "}"
        f.write(f"{line}\n")
        line = (
            "\\newcommand{\\modflowdate}{" + f"{timestamp.strftime('%B %d, %Y')}" + "}"
        )
        f.write(f"{line}\n")
        line = (
            "\\newcommand{\\currentmodflowversion}"
            + "{Version \\modflowversion---\\modflowdate}"
        )
        f.write(f"{line}\n")
        f.close()

    log_update(path, release_type, version)


def update_version_f90(
    release_type: ReleaseType, timestamp: datetime, version: Optional[Version]
):
    path = project_root_path / "src" / "Utilities" / "version.f90"
    lines = open(path, "r").read().splitlines()
    with open(path, "w") as f:
        skip = False
        for line in lines:
            # skip all of the disclaimer text
            if skip:
                if ',/)"' in line:
                    skip = False
                continue
            elif ":: IDEVELOPMODE =" in line:
                line = (
                    "  integer(I4B), parameter :: "
                    + f"IDEVELOPMODE = {1 if release_type == ReleaseType.CANDIDATE else 0}"
                )
            elif ":: VERSIONNUMBER =" in line:
                line = line.rpartition("::")[0] + f":: VERSIONNUMBER = '{version}'"
            elif ":: VERSIONTAG =" in line:
                fmat_tstmp = timestamp.strftime("%m/%d/%Y")
                line = line.rpartition("::")[0] + f":: VERSIONTAG = ' {release_type.value} {fmat_tstmp}'"
            elif ":: FMTDISCLAIMER =" in line:
                line = get_disclaimer(release_type, formatted=True)
                skip = True
            f.write(f"{line}\n")

    log_update(path, release_type, version)


def update_readme_and_disclaimer(
    release_type: ReleaseType, timestamp: datetime, version: Version
):
    disclaimer = get_disclaimer(release_type, formatted=False)
    readme_path = str(project_root_path / "README.md")
    readme_lines = open(readme_path, "r").read().splitlines()
    with open(readme_path, "w") as f:
        for line in readme_lines:
            if "## Version " in line:
                version_line = f"### Version {version}"
                if release_type != ReleaseType.APPROVED:
                    version_line += f" {release_type.value}"
                f.write(f"{version_line}\n")
            elif "Disclaimer" in line:
                f.write(f"{disclaimer}\n")
                break
            else:
                f.write(f"{line}\n")
    log_update(readme_path, release_type, version)

    disclaimer_path = project_root_path / "DISCLAIMER.md"
    with open(disclaimer_path, "w") as f:
        f.write(disclaimer)
    log_update(disclaimer_path, release_type, version)


def update_codejson(release_type: ReleaseType, timestamp: datetime, version: Version):
    path = project_root_path / "code.json"
    with open(path, "r") as f:
        data = json.load(f, object_pairs_hook=OrderedDict)

    data[0]["date"]["metadataLastUpdated"] = timestamp.strftime("%Y-%m-%d")
    data[0]["version"] = str(version)
    data[0]["status"] = release_type.value
    with open(path, "w") as f:
        json.dump(data, f, indent=4)
        f.write("\n")

    log_update(path, release_type, version)


def update_version(
    release_type: ReleaseType = ReleaseType.CANDIDATE,
    timestamp: datetime = datetime.now(),
    version: Version = None,
):
    """
    Update version information stored in version.txt in the project root,
    as well as several other files in the repository. Version updates are
    performed by explicitly providing a version argument to this function
    and a lock is held on the version file to make sure that the state of
    the multiple files containing version information stays synchronized.
    If no version argument is provided, the version number isn't changed.
    """

    lock_path = Path(version_file_path.name + ".lock")
    try:
        lock = FileLock(lock_path)
        previous = Version.from_file(version_file_path)
        version = (
            version
            if version
            else Version(previous.major, previous.minor, previous.patch)
        )

        with lock:
            update_version_txt_and_py(release_type, timestamp, version)
            update_version_tex(release_type, timestamp, version)
            update_version_f90(release_type, timestamp, version)
            update_readme_and_disclaimer(release_type, timestamp, version)
            update_codejson(release_type, timestamp, version)
    finally:
        lock_path.unlink(missing_ok=True)


_initial_version = Version(0, 0, 1)
_current_version = Version.from_file(version_file_path)


@pytest.mark.skip(reason="reverts repo files on cleanup, tread carefully")
@pytest.mark.parametrize(
    "release_type", [ReleaseType.CANDIDATE, ReleaseType.APPROVED]
)
@pytest.mark.parametrize(
    "version",
    [None, Version(major=_initial_version.major, minor=_initial_version.minor, patch=_initial_version.patch)],
)
def test_update_version(tmp_path, release_type, version):
    m_times = [get_modified_time(file) for file in touched_file_paths]
    timestamp = datetime.now()

    try:
        update_version(release_type=release_type, timestamp=timestamp, version=version)
        updated = Version.from_file(version_file_path)

        # check files containing version info were modified
        for p, t in zip(touched_file_paths, m_times):
            assert p.stat().st_mtime > t

        if version:
            # version should be auto-incremented
            assert updated.major == _initial_version.major
            assert updated.minor == _initial_version.minor
            assert updated.patch == _initial_version.patch
        else:
            # version should not have changed
            assert updated.major == _current_version.major
            assert updated.minor == _current_version.minor
            assert updated.patch == _current_version.patch
    finally:
        for p in touched_file_paths:
            os.system(f"git restore {p}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Update Modflow 6 version",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Update version information stored in version.txt in the project root,
            as well as several other files in the repository. If --version is not
            provided, the version number will not be changed. A file lock is held
            to synchronize file access. To indicate a version is production-ready
            use --approve. This will change the disclaimer and version tag label,
            removing 'Release Candidate' from the latter and modifying the former
            to reflect approval The IDEVELOPMODE flag is set to 1 for preliminary
            versions and 0 for approved versions. The version tag must follow the
            '<major>.<minor>.<patch>' format conventions for semantic versioning.
            If --version is provided, --bump-patch, --bump-minor and --bump-major
            may not be provided. Likewise, if any of the latter are provided, the
            version number must not be specified.
            """
        )
    )
    parser.add_argument(
        "-v",
        "--version",
        required=False,
        help="Specify the release version",
    )
    parser.add_argument(
        "-a",
        "--approve",
        required=False,
        action="store_true",
        help="Indicate release is approved (defaults to false for preliminary/development distributions)",
    )
    parser.add_argument(
        "--bump-major",
        required=False,
        action="store_true",
        help="Increment the major version number (cannot be used with --version, defaults to false)",
    )
    parser.add_argument(
        "--bump-minor",
        required=False,
        action="store_true",
        help="Increment the minor version number (cannot be used with --version, defaults to false)",
    )
    parser.add_argument(
        "--bump-patch",
        required=False,
        action="store_true",
        help="Increment the patch version number (cannot be used with --version, defaults to false)",
    )
    parser.add_argument(
        "-g",
        "--get",
        required=False,
        action="store_true",
        help="Get the current version number, don't update anything (defaults to false)",
    )
    args = parser.parse_args()

    if args.version and (args.bump_major or args.bump_minor or args.bump_patch):
        raise ValueError(f"Cannot specify --version and --bump-*, use one or the other")

    bump_flags = [b for b in [args.bump_major, args.bump_minor, args.bump_patch] if b]
    if len(bump_flags) > 1:
        raise ValueError(f"Cannot specify more than one --bump-* flag, use just one")

    if args.get:
        print(Version.from_file(project_root_path / "version.txt"))
    else:
        if not args.version and not any(bump_flags):
            version = _current_version
        elif args.version:
            version = Version.from_string(args.version)
        else:
            current = Version.from_file(project_root_path / "version.txt")
            if args.bump_major:
                print(f"Incrementing major number")
                version = Version(current.major + 1, 0, 0)
            elif args.bump_minor:
                print(f"Incrementing minor number")
                version = Version(current.major, current.minor + 1, 0)
            else:
                print(f"Incrementing patch number")
                version = Version(current.major, current.minor, current.patch + 1)

        update_version(
            release_type=ReleaseType.APPROVED if args.approve else ReleaseType.CANDIDATE,
            timestamp=datetime.now(),
            version=version,
        )
