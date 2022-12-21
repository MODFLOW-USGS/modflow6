import argparse
import os
import platform
import shutil
import textwrap
from _warnings import warn
from datetime import datetime
from os import PathLike
from pathlib import Path
from pprint import pprint
from shutil import which
from tempfile import TemporaryDirectory
from typing import List, Optional
from urllib.error import HTTPError
from warnings import warn

import pytest
from flaky import flaky
from modflow_devtools.build import meson_build
from modflow_devtools.download import list_artifacts, download_artifact, get_release, download_and_unzip
from modflow_devtools.markers import requires_exe, requires_github
from modflow_devtools.misc import set_dir, run_cmd

from benchmark import run_benchmarks
from utils import convert_line_endings
from utils import get_project_root_path

_project_root_path = get_project_root_path()
_version_texf_path = _project_root_path / "doc" / "version.tex"
_examples_repo_path = _project_root_path.parent / "modflow6-examples"
_release_notes_path = _project_root_path / "doc" / "ReleaseNotes"
_distribution_path = _project_root_path / "distribution"
_benchmarks_path = _project_root_path / "distribution" / ".benchmarks"
_docs_path = _project_root_path / "doc"

_default_tex_paths = [
    _project_root_path / "doc" / "mf6io" / "mf6io.tex",
    _project_root_path / "doc" / "ReleaseNotes" / "ReleaseNotes.tex",
    _project_root_path / "doc" / "zonebudget" / "zonebudget.tex",
    _project_root_path / "doc" / "ConverterGuide" / "converter_mf5to6.tex",
    _project_root_path / "doc" / "SuppTechInfo" / "mf6suptechinfo.tex",
]
_system = platform.system()
_eext = ".exe" if _system == "Windows" else ""
_soext = ".dll" if _system == "Windows" else ".so" if _system == "Linux" else ".dylib"


# publications included in distribution docs
_publication_urls = [
    "https://pubs.usgs.gov/tm/06/a55/tm6a55.pdf",
    "https://pubs.usgs.gov/tm/06/a56/tm6a56.pdf",
    "https://pubs.usgs.gov/tm/06/a57/tm6a57.pdf",
    "https://pubs.usgs.gov/tm/06/a61/tm6a61.pdf",
    "https://pubs.usgs.gov/tm/06/a62/tm6a62.pdf",
]


def clean_tex_files():
    print("Cleaning latex files")
    exts = ["pdf", "aux", "bbl", "idx", "lof", "out", "toc"]
    pth = _project_root_path / "doc" / "mf6io"
    files = [(pth / f"mf6io.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = _project_root_path / "doc" / "ReleaseNotes"
    files = [(pth / f"ReleaseNotes.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = _project_root_path / "doc" / "zonebudget"
    files = [(pth / f"zonebudget.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = _project_root_path / "doc" / "ConverterGuide"
    files = [(pth / f"converter_mf5to6.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = _project_root_path.parent / "modflow6-docs.git" / "mf6suptechinfo"
    files = [(pth / f"mf6suptechinfo.{e}") for e in exts]
    if pth.is_dir():
        for file in files:
            file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")

    pth = _examples_repo_path / "doc"
    files = [(pth / f"mf6examples.{e}") for e in exts]
    for file in files:
        file.unlink(missing_ok=True)
    assert not os.path.isfile(str(pth) + ".pdf")


def download_benchmarks(output_path: PathLike, quiet: bool = True) -> Optional[Path]:
    output_path = Path(output_path).expanduser().absolute()
    name = "run-time-comparison"
    repo = "w-bonelli/modflow6"
    artifacts = list_artifacts(repo, name=name, quiet=quiet)
    artifacts = sorted(artifacts, key=lambda a: datetime.strptime(a['created_at'], '%Y-%m-%dT%H:%M:%SZ'), reverse=True)
    most_recent = next(iter(artifacts), None)
    print(f"Found most recent benchmarks (artifact {most_recent['id']})")
    if most_recent:
        print(f"Downloading benchmarks (artifact {most_recent['id']})")
        download_artifact(repo, id=most_recent['id'], path=output_path, quiet=quiet)
        print(f"Downloaded benchmarks to {output_path}")
        path = output_path / f"{name}.md"
        assert path.is_file()
        return path
    else:
        print(f"No benchmarks found")
        return None


@flaky
@requires_github
def test_download_benchmarks(tmp_path):
    path = download_benchmarks(tmp_path, quiet=False)
    if path:
        assert path.name == "run-time-comparison.md"


def build_benchmark_tex(output_path: PathLike, overwrite: bool = False):
    _benchmarks_path.mkdir(parents=True, exist_ok=True)
    benchmarks_path = _benchmarks_path / "run-time-comparison.md"

    # download benchmark artifacts if any exist on GitHub
    if not benchmarks_path.is_file():
        benchmarks_path = download_benchmarks(_benchmarks_path)

    # run benchmarks again if no benchmarks found on GitHub or overwrite requested
    if overwrite or not benchmarks_path.is_file():
        run_benchmarks(
            build_path=_project_root_path / "builddir",
            current_bin_path=_project_root_path / "bin",
            previous_bin_path=_project_root_path / "bin" / "rebuilt",
            examples_path=_examples_repo_path / "examples",
            output_path=output_path)

    # convert markdown benchmark results to LaTeX
    with set_dir(_release_notes_path):
        tex_path = Path("run-time-comparison.tex")
        tex_path.unlink(missing_ok=True)
        out, err, ret = run_cmd("python", "mk_runtimecomp.py", benchmarks_path, verbose=True)
        assert not ret, out + err
        assert tex_path.is_file()

    if (_distribution_path / f"{benchmarks_path.stem}.md").is_file():
        assert (_docs_path / "ReleaseNotes" / f"{benchmarks_path.stem}.tex").is_file()


@flaky
@requires_github
@pytest.mark.skipif(not (_benchmarks_path / "run-time-comparison.md").is_file(), reason="needs benchmarks")
def test_build_benchmark_tex(tmp_path):
    benchmarks_path = _benchmarks_path / "run-time-comparison.md"
    tex_path = _distribution_path / f"{benchmarks_path.stem}.tex"

    try:
        build_benchmark_tex(tmp_path)
        assert benchmarks_path.is_file()
    finally:
        tex_path.unlink(missing_ok=True)


def build_mf6io_tex_from_dfn(overwrite: bool = False):
    if overwrite:
        clean_tex_files()

    def files_match(tex_path, dfn_path, ignored):
        dfn_names = [
            f.stem
            for f in dfn_path.glob("*")
            if f.is_file()
               and "dfn" in f.suffix
               and not any(pattern in f.name for pattern in ignored)
        ]
        tex_names = [
            f.stem.replace("-desc", "")
            for f in tex_path.glob("*")
            if f.is_file()
               and "tex" in f.suffix
               and not any(pattern in f.name for pattern in ignored)
        ]

        return set(tex_names) == set(dfn_names)

    with set_dir(_project_root_path / "doc" / "mf6io" / "mf6ivar"):
        ignored = ["appendix", "common"]
        tex_pth = Path("tex")
        dfn_pth = Path("dfn")
        tex_files = [f for f in tex_pth.glob("*") if f.is_file()]
        dfn_files = [f for f in dfn_pth.glob("*") if f.is_file()]

        if not overwrite and any(tex_files) and any(dfn_files) and files_match(tex_pth, dfn_pth, ignored):
            print(f"DFN files already exist:")
            pprint(dfn_files)
        else:
            for f in tex_files:
                f.unlink()

            # run python script
            out, err, ret = run_cmd("python", "mf6ivar.py")
            assert not ret, out + err

            # check that dfn and tex files match
            assert files_match(tex_pth, dfn_pth, ignored)


@pytest.mark.parametrize("overwrite", [True, False])
def test_build_mf6io_tex_from_dfn(overwrite):
    mf6ivar_path = _project_root_path / "doc" / "mf6io" / "mf6ivar"
    file_paths = [p for p in (mf6ivar_path / "tex").glob("*.tex") if p.is_file()] + [
        mf6ivar_path / "md" / "mf6ivar.md",
        mf6ivar_path / "tex" / "gwf-disv-griddata.dat",
        mf6ivar_path / "tex" / "gwf-npf-options.dat",
    ]
    file_mtimes = [p.stat().st_mtime for p in file_paths]

    try:
        build_mf6io_tex_from_dfn(overwrite=overwrite)

        # files should have been modified if overwrite is true
        for p, t in zip(file_paths, file_mtimes):
            assert overwrite == (p.stat().st_mtime > t)
    finally:
        for p in (file_paths + [
            # should these be under version control, since they're cleaned in fn above?
            _project_root_path / "doc" / "ConverterGuide" / "converter_mf5to6.bbl",
            _project_root_path / "doc" / "ReleaseNotes" / "ReleaseNotes.bbl",
            _project_root_path / "doc" / "mf6io" / "mf6io.bbl",
            _project_root_path / "doc" / "zonebudget" / "zonebudget.bbl"
        ]):
            os.system(f"git restore {p}")


def build_tex_folder_structure(overwrite: bool = False):
    path = _release_notes_path / "folder_struct.tex"

    if overwrite:
        path.unlink(missing_ok=True)
    elif path.is_file():
        print(f"Folder structure file already exists: {path}")
        return

    with set_dir(_release_notes_path):
        out, err, ret = run_cmd("python", "mk_folder_struct.py", "-dp", _project_root_path)
        assert not ret, out + err

    assert path.is_file(), f"Failed to create {path}"


def test_build_tex_folder_structure():
    path = _project_root_path / "doc" / "ReleaseNotes" / "folder_struct.tex"
    try:
        build_tex_folder_structure()
    finally:
        os.system(f"git restore {path}")


def build_mf6io_tex_example(workspace_path: PathLike, bin_path: PathLike, example_model_path: PathLike):
    workspace_path = Path(workspace_path) / "workspace"
    bin_path = Path(bin_path).expanduser().absolute()
    mf6_exe_path = bin_path / f"mf6{_eext}"
    example_model_path = Path(example_model_path).expanduser().absolute()

    assert mf6_exe_path.is_file(), f"{mf6_exe_path} does not exist"
    assert example_model_path.is_dir(), f"{example_model_path} does not exist"

    tex_path = _project_root_path / "doc" / "mf6io"
    fname1 = tex_path / "mf6output.tex"
    fname2 = tex_path / "mf6noname.tex"
    fname3 = tex_path / "mf6switches.tex"
    cmd = str(mf6_exe_path)

    if workspace_path.is_dir():
        shutil.rmtree(workspace_path)
    shutil.copytree(example_model_path, workspace_path)

    # run example model

    with set_dir(workspace_path):
        out, err, ret = run_cmd(cmd)
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
        out, err, ret = run_cmd(cmd)
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
        out, err, ret = run_cmd(str(mf6_exe_path), "-h")
        buff = out + err
        lines = buff.split("\r\n")
        with open(fname3, "w") as f:
            f.write("{\\small\n")
            f.write("\\begin{lstlisting}[style=modeloutput]\n")
            for line in lines:
                f.write(line.rstrip() + "\n")
            f.write("\\end{lstlisting}\n")
            f.write("}\n")


def test_build_mf6io_tex_example():
    pass


def build_pdfs_from_tex(tex_paths: List[PathLike], output_path: PathLike, passes: int = 3, overwrite: bool = False):
    print(f"Building PDFs from LaTex:")
    pprint(tex_paths)

    output_path = Path(output_path).expanduser().absolute()
    built_paths = set()
    for tex_path in tex_paths:
        tex_path = Path(tex_path).expanduser().absolute()
        pdf_name = tex_path.stem + ".pdf"
        pdf_path = tex_path.parent / pdf_name
        tgt_path = output_path / pdf_name
        if overwrite or not tgt_path.is_file():
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
                        out, err, ret = run_cmd("bibtex", tex_path.stem + ".aux")
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

        assert tgt_path.is_file(), f"Failed to build {tgt_path} from {tex_path}"
        assert tgt_path not in built_paths, f"Duplicate target: {tgt_path}"
        built_paths.add(tgt_path)


@requires_exe("pdflatex")
def test_build_pdfs_from_tex(tmp_path):
    tex_paths = [
        _docs_path / "mf6io" / "mf6io.tex",
        _docs_path / "ReleaseNotes" / "ReleaseNotes.tex",
        _docs_path / "zonebudget" / "zonebudget.tex",
        _docs_path / "ConverterGuide" / "converter_mf5to6.tex",
        _docs_path / "SuppTechInfo" / "mf6suptechinfo.tex",
        _examples_repo_path / "doc" / "mf6examples.tex",
    ]
    bbl_paths = [
        _docs_path / "ConverterGuide" / "converter_mf5to6.bbl",
        _docs_path / "ReleaseNotes" / "ReleaseNotes.tex",
        _docs_path / "zonebudget" / "zonebudget.tex",
    ]

    try:
        build_pdfs_from_tex(tex_paths, tmp_path)
    finally:
        for p in (tex_paths[:-1] + bbl_paths):
            os.system(f"git restore {p}")


def build_documentation(bin_path: PathLike,
                        output_path: PathLike,
                        examples_repo_path: PathLike,
                        development: bool = False,
                        overwrite: bool = False):
    print(f"Building {'development' if development else 'candidate'} documentation")

    bin_path = Path(bin_path).expanduser().absolute()
    output_path = Path(output_path).expanduser().absolute()
    examples_repo_path = Path(examples_repo_path).expanduser().absolute()

    # make sure output directory exists
    output_path.mkdir(parents=True, exist_ok=True)

    # build LaTex input/output docs from DFN files
    build_mf6io_tex_from_dfn(overwrite=True)

    # build LaTeX input/output example model docs
    with TemporaryDirectory() as temp:
        temp_path = Path(temp)
        build_mf6io_tex_example(
            workspace_path=temp_path,
            bin_path=bin_path,
            example_model_path=examples_repo_path / "examples" / "ex-gwf-twri01",
        )

    # build LaTeX file describing distribution folder structure
    # build_tex_folder_structure(overwrite=True)

    if development:
        # convert LaTeX to PDF
        build_pdfs_from_tex(tex_paths=[_docs_path / "mf6io" / "mf6io.tex"], output_path=output_path)
    else:
        # convert benchmarks to LaTex, running them first if necessary
        build_benchmark_tex(output_path=output_path, overwrite=overwrite)

        # download example docs
        latest = get_release("MODFLOW-USGS/modflow6-examples", "latest")
        assets = latest["assets"]
        asset = next(iter([a for a in assets if a["name"] == "mf6examples.pdf"]), None)
        download_and_unzip(asset["browser_download_url"], output_path, verbose=True)

        # download publications
        for url in _publication_urls:
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
        build_pdfs_from_tex(tex_paths=_default_tex_paths, output_path=output_path, overwrite=overwrite)

    # enforce os line endings on all text files
    windows_line_endings = True
    convert_line_endings(output_path, windows_line_endings)

    # make sure we have expected PDFs
    if development:
        assert (output_path / "mf6io.pdf").is_file()
    else:
        assert (output_path / "mf6io.pdf").is_file()
        assert (output_path / "ReleaseNotes.pdf").is_file()
        assert (output_path / "zonebudget.pdf").is_file()
        assert (output_path / "converter_mf5to6.pdf").is_file()
        assert (output_path / "mf6suptechinfo.pdf").is_file()
        assert (output_path / "mf6examples.pdf").is_file()


@requires_exe("pdflatex")
@pytest.mark.skip(reason="manual testing")
@pytest.mark.skipif(not (_benchmarks_path / "run-time-comparison.md").is_file(), reason="needs benchmarks")
def test_build_documentation(tmp_path):
    bin_path = tmp_path / "bin"
    dist_path = tmp_path / "dist"
    meson_build(_project_root_path, tmp_path / "builddir", bin_path)
    build_documentation(bin_path, dist_path, _examples_repo_path) #, _benchmarks_path / "run-time-comparison.md")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Convert LaTeX docs to PDFs",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Create documentation for a distribution. This includes benchmarks, release notes, the
            MODFLOW 6 input/output specification, example model documentation, supplemental info,
            documentation for the MODFLOW 5 to 6 converter and Zonebudget 6, and several articles
            downloaded from the USGS website. These are all written to a specified --output-path.
            Additional LaTeX files may be included in the distribution by specifying --tex-paths.
            """
        ),
    )
    parser.add_argument("-t", "--tex-path", action="append", required=False, help="Extra LaTeX files to include")
    parser.add_argument(
        "-b",
        "--bin-path",
        required=False,
        default=os.getcwd(),
        help="Location of modflow6 executables",
    )
    parser.add_argument(
        "-e",
        "--examples-repo-path",
        required=False,
        default=str(_examples_repo_path),
        help="Path to directory containing modflow6 example models"
    )
    parser.add_argument(
        "-o",
        "--output-path",
        required=False,
        default=os.getcwd(),
        help="Location to create documentation artifacts",
    )
    parser.add_argument(
        "-d",
        "--development",
        required=False,
        default=False,
        action="store_true",
        help="Whether to build a development (e.g., nightly) rather than a full distribution"
    )
    parser.add_argument(
        "-f",
        "--force",
        required=False,
        default=False,
        action="store_true",
        help="Whether to recreate and overwrite existing artifacts"
    )
    args = parser.parse_args()
    tex_paths = _default_tex_paths + ([Path(p) for p in args.tex_path] if args.tex_path else [])
    output_path = Path(args.output_path).expanduser().absolute()
    output_path.mkdir(parents=True, exist_ok=True)
    bin_path = Path(args.bin_path).expanduser().absolute()
    examples_repo_path = Path(args.examples_repo_path).expanduser().absolute()

    build_documentation(
        bin_path=bin_path,
        output_path=output_path,
        examples_repo_path=examples_repo_path,
        development=args.development,
        overwrite=args.force)
