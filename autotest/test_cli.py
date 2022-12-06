import subprocess
from pathlib import Path

_project_root_path = Path(__file__).parent.parent
_bin_path = _project_root_path / "bin"


def test_cli_version():
    output = ' '.join(subprocess.check_output([str(_bin_path / 'mf6'), "-v"]).decode().split())
    assert output.startswith("mf6:")
    assert output.lower().count("release") == 1
    # assert output.lower().count("candidate") <= 1

    print(output)

    version = output.lower().rpartition(":")[2].rpartition("release")[0].strip()
    v_split = version.split(".")
    assert len(v_split) == 3
    assert all(s.isdigit() for s in v_split)
