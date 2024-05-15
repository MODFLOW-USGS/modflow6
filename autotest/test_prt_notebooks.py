import re
from os import environ
from pathlib import Path
from platform import system
from pprint import pprint
from warnings import warn

import numpy as np
import pandas as pd
import pytest
from conftest import project_root_path
from flopy.mf6 import MFSimulation
from flopy.utils import PathlineFile
from modflow_devtools.misc import run_cmd, set_env


def get_notebook_scripts(pattern=None, exclude=None):
    repos_path = environ.get("REPOS_PATH", None)
    if repos_path is None:
        repos_path = project_root_path.parent
    repo_path = Path(repos_path) / "modflow6-examples"
    if not repo_path.is_dir():
        return []
    nbpaths = [
        str(p)
        for p in (repo_path / "scripts").glob("*.py")
        if pattern is None or pattern in p.name
    ]

    # sort for pytest-xdist: workers must collect tests in the same order
    return sorted(
        [p for p in nbpaths if not exclude or not any(e in p for e in exclude)]
    )


@pytest.mark.slow
@pytest.mark.parametrize(
    "notebook",
    get_notebook_scripts(pattern="ex-prt", exclude=["ex-prt-mp7-p03"]),
)
def test_notebooks(notebook, function_tmpdir, targets):
    notebook = Path(notebook)

    # temporarily add testing binaries to PATH
    delim = ";" if system() == "Windows" else ":"
    path = (
        environ.get("PATH", "")
        + f"{delim}{targets['mf6'].parent}"
        + f"{delim}{targets['mf6'].parent / 'downloaded'}"
        + f"{delim}{targets['mf6'].parent / 'rebuilt'}"
    )
    with set_env(PATH=path):
        args = [
            "jupytext",
            "--from",
            "py",
            "--to",
            "ipynb",
            "--execute",
            "--run-path",
            function_tmpdir,
            "--output",
            function_tmpdir / f"{notebook.stem}.ipynb",
            str(notebook),
        ]
        # run the notebook
        stdout, stderr, returncode = run_cmd(*args, verbose=True)

    # show output
    pprint(stdout)
    pprint(stderr)

    # check return code
    if returncode != 0:
        if "Missing optional dependency" in stderr:
            pkg = re.findall("Missing optional dependency '(.*)'", stderr)[0]
            pytest.skip(f"notebook requires optional dependency {pkg!r}")
        elif "No module named " in stderr:
            pkg = re.findall("No module named '(.*)'", stderr)[0]
            pytest.skip(f"notebook requires package {pkg!r}")
    assert returncode == 0, f"could not run {notebook}"

    # check results
    example_name = notebook.stem.replace("ex-prt-", "")
    pathlines_file = function_tmpdir / example_name / "prt" / (example_name + "-prt.trk.csv")
    pathlines = pd.read_csv(pathlines_file).round(3).to_records(index=False)
    assert any(pathlines)