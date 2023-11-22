import subprocess
import platform

from conftest import project_root_path

bin_path = project_root_path / "bin"

app = "mf6"
ext = ".exe" if platform.system() == "Windows" else ""
app = f"{app}{ext}"


def test_cli_version():
    output = " ".join(
        subprocess.check_output([str(bin_path / app), "-v"]).decode().split()
    )
    print(output)
    assert output.startswith(f"{app}:"), f"found: {output}"

    version = output.lower().split(" ")[1]
    print(version)
    v_split = version.split(".")
    assert len(v_split) >= 2
    assert all(s[-1].isdigit() for s in v_split[:2])
