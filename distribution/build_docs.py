import argparse
import os
import platform
import shutil
import sys
import textwrap
from datetime import datetime
from os import PathLike, environ
from pathlib import Path
from pprint import pformat, pprint
from tempfile import TemporaryDirectory
from typing import List, Optional
from urllib.error import HTTPError
from warnings import warn

import pytest
from benchmark import run_benchmarks
from flaky import flaky
from modflow_devtools.build import meson_build
from modflow_devtools.download import (
    download_and_unzip,
    download_artifact,
    get_release,
    list_artifacts,
)
from modflow_devtools.markers import no_parallel, requires_exe, requires_github
from modflow_devtools.misc import run_cmd, run_py_script, set_dir

from utils import convert_line_endings, get_project_root_path

# paths
PROJ_ROOT_PATH = get_project_root_path()
BIN_PATH = PROJ_ROOT_PATH / "bin"
EXAMPLES_REPO_PATH = PROJ_ROOT_PATH.parent / "modflow6-examples"
DISTRIBUTION_PATH = PROJ_ROOT_PATH / "distribution"
BENCHMARKS_PATH = PROJ_ROOT_PATH / "distribution" / ".benchmarks"
DOCS_PATH = PROJ_ROOT_PATH / "doc"
MF6IO_PATH = DOCS_PATH / "mf6io"
RELEASE_NOTES_PATH = DOCS_PATH / "ReleaseNotes"
DEPRECATIONS_SCRIPT_PATH = MF6IO_PATH / "mf6ivar" / "deprecations.py"
TEX_PATHS = {
    "minimal": [
        MF6IO_PATH / "mf6io.tex",
        DOCS_PATH / "ReleaseNotes" / "ReleaseNotes.tex",
    ],
    "full": [
        MF6IO_PATH / "mf6io.tex",
        DOCS_PATH / "ReleaseNotes" / "ReleaseNotes.tex",
        DOCS_PATH / "zonebudget" / "zonebudget.tex",
        DOCS_PATH / "ConverterGuide" / "converter_mf5to6.tex",
        DOCS_PATH / "SuppTechInfo" / "mf6suptechinfo.tex",
    ],
}
DEFAULT_MODELS = ["gwf", "gwt", "gwe", "prt", "swf"]

# OS-specific extensions
SYSTEM = platform.system()
EXE_EXT = ".exe" if SYSTEM == "Windows" else ""
LIB_EXT = (
    ".dll" if SYSTEM == "Windows" else ".so" if SYSTEM == "Linux" else ".dylib"
)

# publications included in full dist docs
PUB_URLS = [
    "https://pubs.usgs.gov/tm/06/a55/tm6a55.pdf",
    "https://pubs.usgs.gov/tm/06/a56/tm6a56.pdf",
    "https://pubs.usgs.gov/tm/06/a57/tm6a57.pdf",
    "https://pubs.usgs.gov/tm/06/a61/tm6a61.pdf",
    "https://pubs.usgs.gov/tm/06/a62/tm6a62.pdf",
]


def clean_tex_files():
    """Remove LaTeX files before a clean rebuild."""

    print("Cleaning latex files")
    exts = ["pdf", "aux", "bbl", "idx", "lof", "out", "toc"]
    pth = PROJ_ROOT_PATH / "doc" / "mf6io"
    files = [(pth / f"mf6io.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = PROJ_ROOT_PATH / "doc" / "ReleaseNotes"
    files = [(pth / f"ReleaseNotes.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = PROJ_ROOT_PATH / "doc" / "zonebudget"
    files = [(pth / f"zonebudget.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = PROJ_ROOT_PATH / "doc" / "ConverterGuide"
    files = [(pth / f"converter_mf5to6.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = PROJ_ROOT_PATH.parent / "modflow6-docs.git" / "mf6suptechinfo"
    files = [(pth / f"mf6suptechinfo.{e}") for e in exts]
    if pth.is_dir():
        for file in files:
            file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = EXAMPLES_REPO_PATH / "doc"
    files = [(pth / f"mf6examples.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")


def download_benchmarks(
    output_path: PathLike,
    verbose: bool = False,
    repo_owner: str = "MODFLOW-USGS",
) -> Optional[Path]:
    """Try to download MF6 benchmarks from GitHub Actions."""

    output_path = Path(output_path).expanduser().absolute()
    name = "run-time-comparison"  # todo make configurable
    repo = (
        f"{repo_owner}/modflow6"  # todo make configurable, add pytest/cli args
    )
    artifacts = list_artifacts(repo, name=name, verbose=verbose)
    artifacts = sorted(
        artifacts,
        key=lambda a: datetime.strptime(a["created_at"], "%Y-%m-%dT%H:%M:%SZ"),
        reverse=True,
    )
    artifacts = [
        a
        for a in artifacts
        if a["workflow_run"]["head_branch"]
        == "develop"  # todo make configurable
    ]
    most_recent = next(iter(artifacts), None)
    print(f"Found most recent benchmarks (artifact {most_recent['id']})")
    if most_recent:
        print(f"Downloading benchmarks (artifact {most_recent['id']})")
        download_artifact(
            repo, id=most_recent["id"], path=output_path, verbose=verbose
        )
        print(f"Downloaded benchmarks to {output_path}")
        path = output_path / f"{name}.md"
        assert path.is_file()
        return path
    else:
        print("No benchmarks found")
        return None


@pytest.fixture
def github_user() -> Optional[str]:
    return environ.get("GITHUB_USER", None)


@flaky
@no_parallel
@requires_github
def test_download_benchmarks(tmp_path, github_user):
    path = download_benchmarks(
        tmp_path,
        verbose=True,
        repo_owner=github_user if github_user else "MODFLOW-USGS",
    )
    if path:
        assert path.name == "run-time-comparison.md"


def build_benchmark_tex(
    output_path: PathLike,
    force: bool = False,
    repo_owner: str = "MODFLOW-USGS",
):
    """Build LaTeX files for MF6 performance benchmarks to go into the release notes."""

    BENCHMARKS_PATH.mkdir(parents=True, exist_ok=True)
    benchmarks_path = BENCHMARKS_PATH / "run-time-comparison.md"

    # download benchmark artifacts if any exist on GitHub
    if not benchmarks_path.is_file():
        benchmarks_path = download_benchmarks(
            BENCHMARKS_PATH, repo_owner=repo_owner
        )

    # run benchmarks again if no benchmarks found on GitHub or overwrite requested
    if force or not benchmarks_path.is_file():
        run_benchmarks(
            build_path=PROJ_ROOT_PATH / "builddir",
            current_bin_path=PROJ_ROOT_PATH / "bin",
            previous_bin_path=PROJ_ROOT_PATH / "bin" / "rebuilt",
            examples_path=EXAMPLES_REPO_PATH / "examples",
            output_path=output_path,
        )

    # convert markdown benchmark results to LaTeX
    with set_dir(RELEASE_NOTES_PATH):
        tex_path = Path("run-time-comparison.tex")
        tex_path.unlink(missing_ok=True)
        out, err, ret = run_cmd(
            sys.executable, "mk_runtimecomp.py", benchmarks_path, verbose=True
        )
        assert not ret, out + err
        assert tex_path.is_file()

    if (DISTRIBUTION_PATH / f"{benchmarks_path.stem}.md").is_file():
        assert (
            DOCS_PATH / "ReleaseNotes" / f"{benchmarks_path.stem}.tex"
        ).is_file()


@flaky
@no_parallel
@requires_github
def test_build_benchmark_tex(tmp_path):
    benchmarks_path = BENCHMARKS_PATH / "run-time-comparison.md"
    tex_path = DISTRIBUTION_PATH / f"{benchmarks_path.stem}.tex"

    try:
        build_benchmark_tex(tmp_path)
        assert benchmarks_path.is_file()
    finally:
        tex_path.unlink(missing_ok=True)


def build_deprecations_tex():
    """Build LaTeX files for the deprecations table to go into the release notes."""

    mf6ivar_path = MF6IO_PATH / "mf6ivar"
    md_path = mf6ivar_path / "md"
    md_path.mkdir(exist_ok=True)

    # make deprecations markdown table
    run_py_script(DEPRECATIONS_SCRIPT_PATH)
    with set_dir(mf6ivar_path):
        deprecations_path = md_path / "deprecations.md"
        deprecations_path.unlink(missing_ok=True)
        out, err, ret = run_cmd(
            sys.executable, "deprecations.py", verbose=True
        )
        assert not ret, out + err
        assert deprecations_path.is_file()

    # convert markdown deprecations to LaTeX
    with set_dir(RELEASE_NOTES_PATH):
        tex_path = Path("deprecations.tex")
        tex_path.unlink(missing_ok=True)
        out, err, ret = run_cmd(
            sys.executable,
            "mk_deprecations.py",
            deprecations_path,
            verbose=True,
        )
        assert not ret, out + err
        assert tex_path.is_file()

    assert (
        DOCS_PATH / "ReleaseNotes" / f"{deprecations_path.stem}.tex"
    ).is_file()


@no_parallel
def test_build_deprecations_tex():
    build_deprecations_tex()


def build_mf6io_tex(models: Optional[List[str]] = None, force: bool = False):
    """Build LaTeX files for the MF6IO guide from DFN files."""
    if force:
        clean_tex_files()

    def match(tex_names, dfn_names):
        tex = set(tex_names)
        dfn = set(dfn_names)
        diff = tex ^ dfn
        return not any(diff)

    def assert_match(tex_names, dfn_names):
        tex = set(tex_names)
        dfn = set(dfn_names)
        diff = tex ^ dfn
        assert not any(diff), (
            f"=> symmetric difference:\n{pformat(diff)}\n"
            f"=> tex - dfn:\n{pformat(tex - dfn)}\n"
            f"=> dfn - tex:\n{pformat(dfn - tex)}\n"
        )

    with set_dir(PROJ_ROOT_PATH / "doc" / "mf6io" / "mf6ivar"):
        ignored = ["appendix", "common"] + list(
            set(DEFAULT_MODELS) - set(models)
        )
        included = models + ["sim", "utl", "exg", "sln"]
        tex_files = [
            f
            for f in Path("tex").glob("*.tex")
            if f.is_file()
            and any(pattern in f.name for pattern in included)
            and not any(pattern in f.name for pattern in ignored)
        ]
        dfn_files = [
            f
            for f in Path("dfn").glob("*.dfn")
            if f.is_file()
            and any(pattern in f.name for pattern in included)
            and not any(pattern in f.name for pattern in ignored)
        ]
        dfn_names = [f.stem for f in dfn_files]
        tex_names = [f.stem.replace("-desc", "") for f in tex_files]
        if match(tex_names, dfn_names) and not force:
            print("DFN files already exist:")
            pprint(dfn_files)
        else:
            for f in tex_files:
                f.unlink()

            # run mf6ivar script and make sure a tex
            # file was generated for each dfn
            args = [sys.executable, "mf6ivar.py"]
            if models is not None and any(models):
                for model in models:
                    args += ["--model", model]
            out, err, ret = run_cmd(*args, verbose=True)
            assert not ret, out + err
            assert_match(tex_names, dfn_names)


@no_parallel
@pytest.mark.parametrize("force", [True, False])
def test_build_mf6io_tex(force):
    build_mf6io_tex(force=force)


def build_usage_example_tex(
    workspace_path: PathLike, bin_path: PathLike, example_model_path: PathLike
):
    """
    Build LaTeX files for the MF6 usage example in the MF6IO guide.
    Runs MF6 to capture the output and insert into the document.
    """

    workspace_path = Path(workspace_path) / "workspace"
    bin_path = Path(bin_path).expanduser().absolute()
    mf6_exe_path = bin_path / f"mf6{EXE_EXT}"
    example_model_path = Path(example_model_path).expanduser().absolute()

    assert mf6_exe_path.is_file(), f"{mf6_exe_path} does not exist"
    assert example_model_path.is_dir(), f"{example_model_path} does not exist"

    tex_path = PROJ_ROOT_PATH / "doc" / "mf6io"
    fname1 = tex_path / "mf6output.tex"
    fname2 = tex_path / "mf6noname.tex"
    fname3 = tex_path / "mf6switches.tex"
    cmd = str(mf6_exe_path)

    if workspace_path.is_dir():
        shutil.rmtree(workspace_path)
    shutil.copytree(example_model_path, workspace_path)

    # run example model
    with set_dir(workspace_path):
        out, err, ret = run_cmd(cmd, verbose=True)
        buff = out + err
        lines = buff.split("\r\n")
        with open(fname1, "w") as f:
            f.write("{\\small\n")
            f.write("\\begin{lstlisting}[style=modeloutput]\n")
            for line in lines:
                f.write(line.rstrip() + "\n")
            f.write("\\end{lstlisting}\n")
            f.write("}\n")

    if workspace_path.is_dir():
        shutil.rmtree(workspace_path)
    os.mkdir(workspace_path)

    # run model without a namefile present
    with set_dir(workspace_path):
        out, err, ret = run_cmd(cmd, verbose=True)
        buff = out + err
        lines = buff.split("\r\n")
        with open(fname2, "w") as f:
            f.write("{\\small\n")
            f.write("\\begin{lstlisting}[style=modeloutput]\n")
            for line in lines:
                f.write(line.rstrip() + "\n")
            f.write("\\end{lstlisting}\n")
            f.write("}\n")

    with set_dir(workspace_path):
        # run mf6 command with -h to show help
        out, err, ret = run_cmd(str(mf6_exe_path), "-h", verbose=True)
        buff = out + err
        lines = buff.split("\r\n")
        with open(fname3, "w") as f:
            f.write("{\\small\n")
            f.write("\\begin{lstlisting}[style=modeloutput]\n")
            for line in lines:
                f.write(line.rstrip() + "\n")
            f.write("\\end{lstlisting}\n")
            f.write("}\n")


def build_pdfs(
    tex_paths: List[PathLike],
    output_path: PathLike,
    passes: int = 3,
    force: bool = False,
):
    """Build PDF documents from LaTeX files."""

    print("Building PDFs from LaTex:")
    pprint(tex_paths)

    output_path = Path(output_path).expanduser().absolute()
    built_paths = set()
    for tex_path in tex_paths:
        tex_path = Path(tex_path).expanduser().absolute()
        pdf_name = tex_path.stem + ".pdf"
        pdf_path = tex_path.parent / pdf_name
        tgt_path = output_path / pdf_name
        if force or not tgt_path.is_file():
            print(f"Converting {tex_path} to PDF")
            with set_dir(tex_path.parent):
                first = True
                for i in range(passes):
                    print(f"Pass {i + 1}/{passes}")
                    out, err, ret = run_cmd(
                        "pdflatex",
                        "-interaction=nonstopmode",
                        "-halt-on-error",
                        tex_path.name,
                    )
                    buff = out + err
                    assert not ret, buff
                    if first:
                        out, err, ret = run_cmd(
                            "bibtex", tex_path.stem + ".aux"
                        )
                        buff = out + err
                        assert not ret or "I found no" in buff, buff
                        first = False

            if tgt_path.is_file():
                print(f"Clobbering {tgt_path}")
                tgt_path.unlink()

            print(f"Moving {pdf_path} to {tgt_path}")
            pdf_path.rename(tgt_path)
        else:
            print(f"{tgt_path} already exists, nothing to do")

        assert (
            tgt_path.is_file()
        ), f"Failed to build {tgt_path} from {tex_path}"
        assert tgt_path not in built_paths, f"Duplicate target: {tgt_path}"
        built_paths.add(tgt_path)


@no_parallel
@requires_exe("pdflatex")
def test_build_pdfs_from_tex(tmp_path):
    tex_paths = [
        DOCS_PATH / "mf6io" / "mf6io.tex",
        DOCS_PATH / "ReleaseNotes" / "ReleaseNotes.tex",
        DOCS_PATH / "zonebudget" / "zonebudget.tex",
        DOCS_PATH / "ConverterGuide" / "converter_mf5to6.tex",
        DOCS_PATH / "SuppTechInfo" / "mf6suptechinfo.tex",
    ]
    bbl_paths = [
        DOCS_PATH / "ConverterGuide" / "converter_mf5to6.bbl",
    ]

    build_pdfs(tex_paths, tmp_path)
    for p in tex_paths[:-1] + bbl_paths:
        assert p.is_file()


def build_documentation(
    bin_path: PathLike,
    force: bool = False,
    full: bool = False,
    models: Optional[List[str]] = None,
    output_path: Optional[PathLike] = None,
    repo_owner: str = "MODFLOW-USGS",
):
    """Build documentation for a MODFLOW 6 distribution."""

    print(f"Building {'full' if full else 'minimal'} documentation")

    bin_path = Path(bin_path).expanduser().absolute()
    output_path = Path(output_path).expanduser().absolute()

    if (output_path / "mf6io.pdf").is_file() and not force:
        print(f"{output_path / 'mf6io.pdf'} already exists")
        return

    # make sure output directory exists
    output_path.mkdir(parents=True, exist_ok=True)

    # build LaTex input/output docs from DFN files
    build_mf6io_tex(force=force, models=models)

    # build LaTeX input/output example model docs
    with TemporaryDirectory() as temp:
        build_usage_example_tex(
            bin_path=bin_path,
            workspace_path=Path(temp),
            example_model_path=PROJ_ROOT_PATH / ".mf6minsim",
        )

    # build deprecations table for insertion into LaTex release notes
    build_deprecations_tex()

    if full:
        # convert benchmarks to LaTex, running them first if necessary
        build_benchmark_tex(
            output_path=output_path, force=force, repo_owner=repo_owner
        )

        # download example docs
        pdf_name = "mf6examples.pdf"
        if force or not (output_path / pdf_name).is_file():
            latest = get_release(f"{repo_owner}/modflow6-examples", "latest")
            assets = latest["assets"]
            asset = next(
                iter([a for a in assets if a["name"] == pdf_name]), None
            )
            download_and_unzip(
                asset["browser_download_url"], output_path, verbose=True
            )

        # download publications
        for url in PUB_URLS:
            print(f"Downloading publication: {url}")
            try:
                download_and_unzip(url, path=output_path, delete_zip=False)
                assert (output_path / url.rpartition("/")[2]).is_file()
            except HTTPError as e:
                if "404" in str(e):
                    warn(f"Publication not found: {url}")
                else:
                    raise

        # convert LaTex to PDF
        build_pdfs(
            tex_paths=TEX_PATHS["full"],
            output_path=output_path,
            force=force,
        )
    else:
        # just convert LaTeX to PDF
        build_pdfs(
            tex_paths=TEX_PATHS["minimal"],
            output_path=output_path,
            force=force,
        )

    # enforce os line endings on all text files
    windows_line_endings = True
    convert_line_endings(output_path, windows_line_endings)

    # make sure we have expected PDFs
    assert (output_path / "mf6io.pdf").is_file()
    if full:
        assert (output_path / "mf6io.pdf").is_file()
        assert (output_path / "ReleaseNotes.pdf").is_file()
        assert (output_path / "zonebudget.pdf").is_file()
        assert (output_path / "converter_mf5to6.pdf").is_file()
        assert (output_path / "mf6suptechinfo.pdf").is_file()
        assert (output_path / "mf6examples.pdf").is_file()


@no_parallel
@requires_exe("pdflatex")
def test_build_documentation(tmp_path):
    bin_path = tmp_path / "bin"
    dist_path = tmp_path / "dist"
    meson_build(PROJ_ROOT_PATH, tmp_path / "builddir", bin_path)
    build_documentation(bin_path, dist_path, EXAMPLES_REPO_PATH)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Create documentation for a distribution. By default, this only includes the mf6io PDF
            document. If the --full flag is provided this includes benchmarks, release notes, the
            MODFLOW 6 input/output specification, example model documentation, supplemental info,
            documentation for the MODFLOW 5 to 6 converter and Zonebudget 6, and several articles
            downloaded from the USGS website. These are all written to a specified --output-path.
            Additional LaTeX files may be included in the distribution by specifying --tex-paths.
            """
        ),
    )
    parser.add_argument(
        "-b",
        "--bin-path",
        required=False,
        default=str(BIN_PATH),
        help="Location of modflow6 executables",
    )
    parser.add_argument(
        "-f",
        "--force",
        required=False,
        default=False,
        action="store_true",
        help="Recreate and overwrite existing artifacts",
    )
    parser.add_argument(
        "--full",
        required=False,
        default=False,
        action="store_true",
        help="Build docs for a full rather than minimal distribution",
    )
    parser.add_argument(
        "-o",
        "--output-path",
        required=False,
        default=os.getcwd(),
        help="Location to create documentation artifacts",
    )
    parser.add_argument(
        "--repo-owner",
        required=False,
        default="MODFLOW-USGS",
        help="Repository owner (substitute your own for a fork)",
    )
    parser.add_argument(
        "-m",
        "--model",
        required=False,
        action="append",
        help="Filter model types to include",
    )
    args = parser.parse_args()
    output_path = Path(args.output_path).expanduser().absolute()
    output_path.mkdir(parents=True, exist_ok=True)
    bin_path = Path(args.bin_path).expanduser().absolute()
    models = args.model if args.model else DEFAULT_MODELS
    build_documentation(
        bin_path=bin_path,
        force=args.force,
        full=args.full,
        models=models,
        output_path=output_path,
        repo_owner=args.repo_owner,
    )
