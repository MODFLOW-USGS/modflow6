import re
from os import environ
from pathlib import Path
from platform import system
from pprint import pprint
from warnings import warn

import numpy as np
import pandas as pd
import pytest
from flopy.mf6 import MFSimulation
from flopy.plot.plotutil import to_mp7_pathlines
from flopy.utils import PathlineFile
from modflow_devtools.misc import run_cmd, set_env

from conftest import project_root_path


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

    # define expected simulation and model names
    simname = Path(notebook).stem.replace("ex-prt-", "")
    gwfname = f"{simname}-gwf"
    prtname = f"{simname}-prt"
    mp7_name = f"{simname}-mp7"

    # if example working directory doesn't exist, return early
    example_ws = function_tmpdir.parent / "examples" / notebook.stem
    if not example_ws.is_dir():
        warn(f"example workspace {example_ws} does not exist")
        return

    # define working subdirs
    mf6ws = example_ws / "mf6"
    gwfws = example_ws / "gwf"
    prtws = example_ws / "prt"
    mp7ws = example_ws / "mp7"

    # prt notebooks running gwf and prt in separate simulations
    # use gwf, prt and mp7 subdirectories, notebooks with gwf &
    # prt in the same simulation use mf6 and mp7 subdirectories.
    if mf6ws.is_dir():
        gwfws = mf6ws
        prtws = mf6ws
    else:
        assert gwfws.is_dir()
        # there may be more than one prt subdirectory
        prtws = list(example_ws.glob("prt*"))
        assert any(prtws)
        if len(prtws) == 1:
            prtws = prtws[0]

    # load model grid
    gwfsim = MFSimulation.load(sim_ws=gwfws, load_only="dis")
    gwf = gwfsim.get_model(gwfname)
    grid = gwf.modelgrid

    # check gwf output files exist
    gwf_budget_file = gwfws / f"{gwfname}.cbb"
    gwf_head_file = gwfws / f"{gwfname}.hds"
    assert gwf_budget_file.is_file()
    assert gwf_head_file.is_file()

    # initialize PRT pathlines dataframe (loaded below)
    prt_pls = None

    # check prt track output files exist
    if isinstance(prtws, Path):
        prt_track_file = prtws / f"{prtname}.trk"
        prt_track_csv_file = prtws / f"{prtname}.trk.csv"
        assert prt_track_file.is_file()
        assert prt_track_csv_file.is_file()
        prt_pls = pd.read_csv(prt_track_csv_file, na_filter=False)
    else:
        for ws in prtws:
            ll = ws.stem[-1]  # todo append to filename like mp7?
            prt_track_file = ws / f"{prtname}.trk"
            prt_track_csv_file = ws / f"{prtname}.trk.csv"
            assert prt_track_file.is_file()
            assert prt_track_csv_file.is_file()

            # if multiple prt dirs and track files, concatenate them into a single dataframe
            pls = pd.read_csv(prt_track_csv_file, na_filter=False)
            print(f"Adding {pls.shape} pathlines from {prt_track_csv_file}")
            prt_pls = pls if prt_pls is None else pd.concat([prt_pls, pls])

    # initialize MP7 pathlines dataframe (loaded below)
    mp7_pls = None

    # there may be more than one mp7 subdirectory
    mp7ws = list(example_ws.glob("mp7*"))
    assert any(mp7ws)
    if len(mp7ws) == 1:
        mp7ws = mp7ws[0]

    # check mp7 pathline output file(s)
    if isinstance(mp7ws, Path):
        mp7_pathline_file = mp7ws / f"{mp7_name}.mppth"
        assert mp7_pathline_file.is_file()
        mp7_pls = pd.DataFrame.from_records(
            PathlineFile(mp7_pathline_file).get_destination_pathline_data(
                range(grid.nnodes), to_recarray=True
            ),
        )
    else:
        for ws in mp7ws:
            ll = ws.stem[-1]
            mp7_pathline_file = ws / f"{mp7_name}{ll}.mppth"
            mp7_endpoint_file = ws / f"{mp7_name}{ll}.mpend"
            assert mp7_pathline_file.is_file() or mp7_endpoint_file.is_file()

            # if multiple mp7 dirs & pathline files, concatenate them into a single dataframe
            if mp7_pathline_file.is_file():
                pls = pd.DataFrame.from_records(
                    PathlineFile(
                        mp7_pathline_file
                    ).get_destination_pathline_data(
                        range(grid.nnodes), to_recarray=True
                    ),
                )
                print(f"Adding {pls.shape} pathlines from {mp7_pathline_file}")
                mp7_pls = pls if mp7_pls is None else pd.concat([mp7_pls, pls])

    # convert prt results to mp7 format
    prt_pls = to_mp7_pathlines(prt_pls)

    # standardize to one-based indexing
    if mp7_pls.particlegroup.min() == 0:
        mp7_pls.particlegroup = mp7_pls.particlegroup + 1
    if prt_pls.particlegroup.min() == 0:
        prt_pls.particlegroup = prt_pls.particlegroup + 1
    if mp7_pls.node.min() == 0:
        mp7_pls.node = mp7_pls.node + 1
    if prt_pls.node.min() == 0:
        prt_pls.node = prt_pls.node + 1
    if mp7_pls.k.min() == 0:
        mp7_pls.k = mp7_pls.k + 1
    if prt_pls.k.min() == 0:
        prt_pls.k = prt_pls.k + 1

    # drop columns for which there is no direct correspondence between mf6 and mp7
    del prt_pls["sequencenumber"]
    del prt_pls["particleidloc"]
    del prt_pls["xloc"]
    del prt_pls["yloc"]
    del prt_pls["zloc"]
    del prt_pls[
        "node"
    ]  # mp7 node numbers reversed in y direction for disv grids
    del mp7_pls["sequencenumber"]
    del mp7_pls["particleidloc"]
    del mp7_pls["xloc"]
    del mp7_pls["yloc"]
    del mp7_pls["zloc"]
    del mp7_pls["node"]

    # drop values for which time is a whole number (and not 0)
    # (kluge to exclude timeseries data until prt supports it)
    prt_pls = prt_pls[(prt_pls.time % 1 != 0) | (prt_pls.time == 0)]
    mp7_pls = mp7_pls[(mp7_pls.time % 1 != 0) | (mp7_pls.time == 0)]

    # for mp7 example 1 drop prt pathline data for which z = 400
    # (kluge to work around particles starting at water table??)
    if "ex-prt-mp7-p01" in notebook.name:
        prt_pls = prt_pls[prt_pls.z != 400]

    # sort both dataframes by particleid and time
    cols = ["particleid", "time", "x", "y", "z"]
    prt_pls.sort_values(by=cols, inplace=True)
    mp7_pls.sort_values(by=cols, inplace=True)

    # drop duplicates
    prt_pls = prt_pls.drop_duplicates(subset=cols)
    mp7_pls = mp7_pls.drop_duplicates(subset=cols)
    prt_pls = prt_pls.sort_values(by=cols)
    mp7_pls = mp7_pls.sort_values(by=cols)

    # compare result shape
    assert prt_pls.shape == mp7_pls.shape

    # skip comparison for ex-prt-mp7-p02 until mp7 and prt particleids can be guaranteed to correspond
    if "ex-prt-mp7-p02" in notebook.name:
        return

    # compare result equality
    assert np.allclose(prt_pls, mp7_pls, atol=1e-3)
