import re
import subprocess

from conftest import project_root_path

bin_path = project_root_path / "bin"


def split_on_letter(s):
    # match = re.compile("[^\W\d]").search(s)
    match = re.compile("[^0-9]").search(s)
    return [s[:match.start()], s[match.start():]] if match else s


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
    assert len(v_split) == 3
    assert all(s.isdigit() for s in v_split[:2])
    sol = split_on_letter(v_split[2])
    assert sol[0].isdigit()