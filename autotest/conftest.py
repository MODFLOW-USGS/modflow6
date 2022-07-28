import pytest

try:
    from modflow_devtools import (
        MFTestContext,
    )

    modflow6_devbin = "../bin"

    def pytest_sessionstart(session):
        MFTestContext(testbin=modflow6_devbin)

    @pytest.fixture(scope="session")
    def mf6testctx(request):
        return MFTestContext(testbin=modflow6_devbin)

except:
    @pytest.fixture(scope="session")
    def mf6testctx(request):
        return None
