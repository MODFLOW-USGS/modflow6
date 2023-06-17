from pathlib import Path

from update_version import Version

_project_root_path = Path(__file__).resolve().parent.parent
_dist_dir_path = _project_root_path.parent / f"mf{str(Version.from_file(_project_root_path / 'version.txt'))}"


def pytest_addoption(parser):
    # both options below are for check_dist.py
    parser.addoption("-P", "--path", action="store", default=str(_dist_dir_path))
    parser.addoption("-A", "--approved", action="store_true", default=False)
