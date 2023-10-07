import os
import sys
import argparse
import glob
from pathlib import Path
from subprocess import run

# MODFLOW 6 repository directories to check (relative to root)
searchpaths = ["src", "srcbmi", "utils/zonebudget/src"]

# Exclude these directories from checks
excludedirs = [
    "src/Utilities/Libraries/blas",  # external library blas
    "src/Utilities/Libraries/daglib",  # external library dag
    "src/Utilities/Libraries/rcm",  # external library rcm
    "src/Utilities/Libraries/sparsekit",  # external library sparsekit
    "src/Utilities/Libraries/sparskit2",  # external library sparsekit2
]

# Exclude these files from checks
excludefiles = ["src/Utilities/InputOutput.f90"]  # excluded until refactored


class FortranFormatCheck:
    """
    Verify MODFLOW 6 fortran source code format
    """

    def __init__(self, root: Path, verbose: bool):
        self._checkcount = 0
        self._fprettifyfails = []
        self._exclude_dirs = []
        self._exclude_files = []
        self._root = root.resolve()
        self._verbose = verbose
        self._entrypath = Path().cwd()

        os.chdir(self._root)

    def add_search_path(self, path: Path) -> None:
        p = Path(path)

        for f in p.glob("**/*.[fF]9[05]"):
            self._check_src_fprettify(f)

    def add_exclude_dirs(self, excl_dirs: list) -> None:
        self._exclude_dirs += excl_dirs

    def add_exclude_files(self, excl_files: list) -> None:
        self._exclude_files += excl_files

    def clear_exclude_dirs(self) -> None:
        self._exclude_dirs = None
        self._exclude_dirs = []

    def clear_exclude_files(self) -> None:
        self._exclude_files = None
        self._exclude_files = []

    def report(self) -> None:
        print(f"\nFortran source files checked: {self._checkcount}")
        print(f"Fortran source files failed: {len(self._fprettifyfails)}\n")

        for f in self._fprettifyfails:
            print(f"fprettify -c .fprettify.yaml {f}")

        print()

    def exit(self) -> int:
        os.chdir(self._entrypath)

        if len(self._fprettifyfails):
            return 1

        return 0

    def _check_src_fprettify(self, path: Path) -> None:
        if self._excluded(path):
            return

        self._checkcount += 1

        if self._verbose:
            print(path)

        cmd = f"fprettify -d -c .fprettify.yaml {path}"
        result = run(cmd, capture_output=True, shell=True)

        if result.stdout or result.stderr:
            self._fprettifyfails.append(path)

    def _excluded(self, path: Path) -> bool:
        for f in self._exclude_files:
            if os.path.exists(f) and os.path.samefile(path, f):
                return True

        for d in self._exclude_dirs:
            if os.path.exists(d) and os.path.samefile(path.parents[0], d):
                return True

        return False


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "MODFLOW 6 fortran format source code verification"
    )
    parser.add_argument(
        "-r", "--root", help="path to MODFLOW 6 repository root directory"
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="verbose")
    args = parser.parse_args()

    # set MODFLOW 6 repository root
    root = Path(args.root).resolve() if args.root else Path(".").resolve()

    fformat_check = FortranFormatCheck(root=root, verbose=args.verbose)
    fformat_check.add_exclude_dirs(excl_dirs=excludedirs)
    fformat_check.add_exclude_files(excl_files=excludefiles)

    for path in searchpaths:
        fformat_check.add_search_path(path=path)

    fformat_check.report()
    sys.exit(fformat_check.exit())
