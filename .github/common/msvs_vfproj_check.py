import xml.etree.ElementTree as ET
from pathlib import Path


def get_source_files(src_folder):
    p = Path(".")
    src_files = []
    print(f"Processing {src_folder} folder")
    ftypes = ("*.[fF]9[05]", "*.inc")
    src_files = []
    for ft in ftypes:
        src_files.extend(p.glob(f"{src_folder}/**/{ft}"))
    return src_files


def get_msvs_files(vfproj_file):
    print(f"Processing {vfproj_file}")
    tree = ET.parse(vfproj_file)
    root = tree.getroot()
    msvs_files = []
    for f in root.iter("File"):
        s = f.attrib["RelativePath"]
        s = s.replace("\\", "/")
        s = s.replace("../", "")
        fpath = Path(s)
        msvs_files.append(fpath)
    return msvs_files


def check_files(name, src_files, msvs_files):
    print(
        f"Verifying {name} files referenced in msvs project files are in src folder..."
    )
    s, m = set(src_files), set(msvs_files)
    diff = s ^ m
    from pprint import pformat

    assert not any(diff), (
        f"{name} src files don't match msvs project file\n"
        f"=> symmetric difference:\n{pformat(diff)}\n"
        f"=> src - msvs:\n{pformat(s - m)}\n"
        f"=> msvs - src:\n{pformat(m - s)}\n"
        "Check to make sure msvs project file is consistent with source files."
    )


def check_mf6():
    # get list of source files and files referenced in msvs project files
    src_files = get_source_files("src")
    msvs_files = []
    for vfproj in ["./msvs/mf6core.vfproj", "./msvs/mf6.vfproj"]:
        msvs_files.extend(get_msvs_files(vfproj))
    check_files("MF6", src_files, msvs_files)


def check_bmi():
    # get list of source files and files referenced in msvs project files
    src_files = get_source_files("srcbmi")
    msvs_files = []
    for vfproj in ["./msvs/mf6bmi.vfproj"]:
        msvs_files.extend(get_msvs_files(vfproj))
    check_files("BMI", src_files, msvs_files)


if __name__ == "__main__":
    check_mf6()
    check_bmi()
    print("msvs project (vfproj) files appear up-to-date...")
