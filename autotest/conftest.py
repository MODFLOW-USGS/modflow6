import pytest
from pathlib import Path
from shutil import copytree, rmtree, which
import re

try:
    from modflow_devtools import (
        MFTestContext,
    )

    modflow6_devbin = "../bin"

    def pytest_sessionstart(session):
        # setup devtools if not already done
        MFTestContext(testbin=modflow6_devbin)

    @pytest.fixture(scope="session")
    def mf6testctx(request):
        return MFTestContext(testbin=modflow6_devbin)

except:

    @pytest.fixture(scope="session")
    def mf6testctx(request):
        return None


@pytest.fixture(scope="function")
def tmpdir(tmpdir_factory, request) -> Path:
    node = (
        request.node.name.replace("/", "_")
        .replace("\\", "_")
        .replace(":", "_")
    )
    temp = Path(tmpdir_factory.mktemp(node))
    yield Path(temp)

    keep = request.config.getoption("--keep")
    if keep:
        tokens = re.split("\[|\]", temp.name)
        if len(tokens) == 1:
            copytree(temp, Path(keep) / tokens[0], dirs_exist_ok=True)
        else:
            copytree(
                temp,
                Path(keep) / tokens[0] / tokens[1].split("-", 1)[1],
                dirs_exist_ok=True,
            )


def pytest_addoption(parser):
    parser.addoption(
        "--keep",
        action="store",
        default=None,
        help="Save test outputs in named directory path",
    )
