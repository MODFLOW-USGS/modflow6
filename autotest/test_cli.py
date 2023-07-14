import subprocess

from conftest import project_root_path

bin_path = project_root_path / "bin"


def test_cli_version():
    output = " ".join(
        subprocess.check_output([str(bin_path / "mf6"), "-v"]).decode().split()
    )
    print(output)
    assert output.startswith("mf6:")

    version = (
        output.lower().split(' ')[1]
    )
    print(version)
    v_split = version.split(".")
    assert len(v_split) >= 2
    assert all(s[-1].isdigit() for s in v_split[:2])
