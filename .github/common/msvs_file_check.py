from pathlib import Path
import xml.etree.ElementTree as ET


def get_source_files(src_folder):
    p = Path(".")
    src_files = []
    print(f"Processing {src_folder} folder")
    ftypes = ('*.[fF]9[05]', '*.inc')
    src_files = []
    for ft in ftypes:
        src_files.extend(p.glob(f"{src_folder}/**/{ft}"))
    return src_files


def get_msvs_files(vfproj_file):
    print(f"Processing {vfproj_file}")
    tree = ET.parse(vfproj_file)
    root = tree.getroot()
    msvs_files = []
    for f in root.iter('File'):
        s = f.attrib["RelativePath"]
        s = s.replace("\\", "/")
        s = s.replace("../", "")
        fpath = Path(s)
        msvs_files.append(fpath)
    return msvs_files


if __name__ == "__main__":

    # get list of source files and files in msvs project files
    src_files = get_source_files("src")
    msvs_files = []
    for vfproj in ["./msvs/mf6core.vfproj", "./msvs/mf6.vfproj"]:
        msvs_files.extend(get_msvs_files(vfproj))

    print(f"Verifying src files are in msvs project files...")
    number_failures = 0
    for f in src_files:
        if f not in msvs_files:
            print(f"{f} not found in msvs project file")
            number_failures += 1

    print(f"Verifying msvs project files are in src folder...")
    for f in msvs_files:
        if f not in src_files:
            print(f"{f} not found in src folder")
            number_failures += 1

    assert number_failures == 0, "msvs project files not up to date..."
    print ("msvs project files appear up to date...")