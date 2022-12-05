import platform
import shutil
import subprocess
import sys
from datetime import datetime
from os import PathLike, environ
from pathlib import Path
from warnings import warn

from modflow_devtools.markers import requires_exe

_project_root_path = Path(__file__).parent.parent


def get_project_root_path():
    return _project_root_path


def get_modified_time(path: Path) -> float:
    return path.stat().st_mtime if path.is_file() else datetime.today().timestamp()


def get_ostag():
    zipname = sys.platform.lower()
    if zipname == "linux2":
        zipname = "linux"
    elif zipname == "darwin":
        zipname = "mac"
    elif zipname == "win32":
        if platform.architecture()[0] == "64bit":
            zipname = "win64"
    return zipname


def get_repo_path() -> Path:
    """
    Returns the path to the folder containing example/test model repositories.
    """
    repo_path = environ.get("REPOS_PATH", None)
    if not repo_path:
        warn(
            f"REPOS_PATH environment variable missing, defaulting to parent of project root"
        )
    return Path(repo_path) if repo_path else Path(__file__).parent.parent.parent


def copytree(src: PathLike, dst: PathLike, symlinks=False, ignore=None):
    """
    Copy a folder from src to dst.  If dst does not exist, then create it.
    """
    src = Path(src).expanduser().absolute()
    dst = Path(dst).expanduser().absolute()

    for s in src.glob("*"):
        d = dst / s.name
        if s.is_dir():
            print(f"  copying {s} ===> {d}")
            shutil.copytree(s, d, symlinks, ignore)
        else:
            print(f"  copying {s} ===> {d}")
            shutil.copy2(s, d)


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


@requires_exe("dos2unix", "unix2dos")
def test_convert_line_endings():
    pass
