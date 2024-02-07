from pathlib import Path

from packaging.version import Version

_project_root_path = Path(__file__).resolve().parent.parent
_dist_dir_path = (
    _project_root_path.parent
    / f"mf{str(Version((_project_root_path / 'version.txt').read_text().strip()))}"
)


def pytest_addoption(parser):
    # all options below are for check_dist.py
    parser.addoption("-P", "--path", action="store", default=str(_dist_dir_path))
    parser.addoption("-A", "--approved", action="store_true", default=False)
    parser.addoption("-R", "--releasemode", action="store_true", default=False)
    parser.addoption("-F", "--full", action="store_true", default=False)
