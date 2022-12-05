from pathlib import Path

from update_version import Version

_project_root_path = Path(__file__).parent.parent
_dist_dir_path = Path(__file__).parent.parent.parent / f"mf{str(Version.from_file(_project_root_path / 'version.txt'))}"


def pytest_addoption(parser):
    parser.addoption("-P", "--path", action="store", default=str(_dist_dir_path))
