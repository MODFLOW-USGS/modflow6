import os

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)


def get_select_dirs(select_dirs, dirs):
    found_dirs = []
    for d in select_dirs:
        if d.endswith("*"):
            for test_dir in dirs:
                if test_dir.startswith(d.replace("*", "")):
                    found_dirs.append(test_dir)
        elif d.endswith("+"):
            dd = d.replace("+", "")
            for test_dir in dirs:
                sorted_list = sorted([dd, test_dir], reverse=True)
                if sorted_list[0] == test_dir:
                    found_dirs.append(test_dir)
        elif d.endswith("-"):
            dd = d.replace("-", "")
            for test_dir in dirs:
                sorted_list = sorted([dd, test_dir])
                if sorted_list[0] == test_dir or dd in sorted_list[0]:
                    found_dirs.append(test_dir)
        else:
            if d in dirs:
                found_dirs.append(d)

    return found_dirs


def get_select_packages(select_packages, exdir, dirs):
    found_dirs = []
    for d in dirs:
        pth = os.path.join(exdir, d)
        namefiles = pymake.get_namefiles(pth)
        ftypes = []
        for namefile in namefiles:
            ftype = pymake.get_mf6_ftypes(namefile, select_packages)
            if ftype not in ftypes:
                ftypes += ftype
        if len(ftypes) > 0:
            ftypes = [item.upper() for item in ftypes]
            for pak in select_packages:
                if pak in ftypes:
                    found_dirs.append(d)
                    break
    return found_dirs
