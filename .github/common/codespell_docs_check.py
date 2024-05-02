import argparse
import glob
import os
import sys
from pathlib import Path
from subprocess import run

# MODFLOW 6 repository directories to check (relative to root)
searchpaths = [
    "doc",
    ".hpc",
    ".vscode",
    "distribution",
]

# Exclude these directories from checks
excludedirs = [
    "srcbmi/latex",
]

# Exclude these files from checks
excludefiles = []  # add excluded files here


class CodespellCheck:
    """
    Verify MODFLOW 6 fortran source code format
    """

    def __init__(self, root: Path, verbose: bool, write_changes: bool):
        self._checkcount = 0
        self._codespellfails = []
        self._exclude_dirs = []
        self._exclude_files = []
        self._root = root.resolve()
        self._verbose = verbose
        self.write_changes = write_changes
        self._entrypath = Path().cwd()

        os.chdir(self._root)

    def add_search_paths(self) -> None:
        files = []
        files += Path(".").glob("*.md")

        for dir_path in searchpaths:
            p = Path(dir_path)
            for e in ("**/*.dfn", "**/*.tex", "**/*.md"):
                files += p.glob(e)

        for f in sorted(files):
            self._check_docs_codespell(f)

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
        print(f"\nDocument files checked: {self._checkcount}")
        print(
            f"Document files codespell failures: {len(self._codespellfails)}\n"
        )

        if len(self._codespellfails) > 0:
            print(f"codespell failures\n{71*'-'}")
            for f in self._codespellfails:
                print(f"codespell -w {f} --ignore-words=.codespell.ignore")
            print()

    def exit(self) -> int:
        os.chdir(self._entrypath)

        if len(self._codespellfails):
            return 1

        return 0

    def _check_docs_codespell(self, path: Path) -> None:
        if self._excluded(path):
            return

        self._checkcount += 1

        if self._verbose:
            print(f"{self._checkcount: 5d}: {path}")

        if self.write_changes:
            wc_str = "-w"
        else:
            wc_str = ""

        cmd = f"codespell {wc_str} {path} --ignore-words=.codespell.ignore"
        result = run(cmd, capture_output=True, shell=True)

        if result.stdout or result.stderr:
            self._codespellfails.append(path)

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
        "MODFLOW 6 Documents spell check with codespell verification"
    )
    parser.add_argument(
        "-r",
        "--root",
        help="path to MODFLOW 6 repository root directory",
    )
    parser.add_argument(
        "-w",
        "--write-changes",
        help="write changes in place if possible",
        action="store_true",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="verbose",
    )
    args = parser.parse_args()

    # set MODFLOW 6 repository root
    root = Path(args.root).resolve() if args.root else Path(".").resolve()

    doccheck = CodespellCheck(
        root=root,
        verbose=args.verbose,
        write_changes=args.write_changes,
    )
    doccheck.add_exclude_dirs(excl_dirs=excludedirs)
    doccheck.add_exclude_files(excl_files=excludefiles)

    doccheck.add_search_paths()

    doccheck.report()
    sys.exit(doccheck.exit())
