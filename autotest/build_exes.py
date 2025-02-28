import argparse
from pathlib import Path

import pytest
from conftest import project_root_path
from modflow_devtools.build import meson_build

repository = "MODFLOW-ORG/modflow6"
top_bin_path = project_root_path / "bin"


@pytest.fixture
def bin_path():
    return top_bin_path


def test_meson_build(bin_path):
    meson_build(
        project_path=project_root_path,
        build_path=project_root_path / "builddir",
        bin_path=bin_path,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Rebuild local development version of MODFLOW 6")
    parser.add_argument(
        "-p", "--path", help="path to bin directory", default=top_bin_path
    )
    args = parser.parse_args()
    test_meson_build(Path(args.path).expanduser().resolve())
