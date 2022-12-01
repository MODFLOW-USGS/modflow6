import os
import sys

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


def get_home_dir():
    # determine if CI run
    is_CI = "CI" in os.environ

    home = os.path.expanduser("~")

    if is_CI:
        if sys.platform.lower() == "win32":
            home = os.path.normpath(os.path.join(os.getcwd(), "..", ".."))
    else:
        cwd_pth = os.getcwd()

        # convert current working directory to a list
        cwd_list = cwd_pth.split(sep=os.path.sep)

        # add leading path separator back into list
        for idx, pth in enumerate(cwd_list):
            if len(pth) < 1:
                cwd_list[idx] = os.path.sep
            if pth.endswith(":") and sys.platform.lower() == "win32":
                cwd_list[idx] += os.path.sep

        ipos = 0
        for idx, s in enumerate(cwd_list):
            if s.lower().startswith("modflow6"):
                ipos = idx
                break

        home = os.path.join(*cwd_list[:ipos])

    print(f"HOME: {home}")

    return home


def set_mf6_regression():
    mf6_regression = True
    for arg in sys.argv:
        if arg.lower() in ("--original_regression", "-oreg"):
            mf6_regression = False
            break
    return mf6_regression


def is_directory_available(example_basedir):
    available = False
    if example_basedir is not None:
        available = os.path.isdir(example_basedir)
    if not available:
        print(f'"{example_basedir}" does not exist')
        print(f"no need to run {os.path.basename(__file__)}")
    return available


def get_example_basedir(home, find_dir, subdir=None):
    example_basedir = None
    for root, dirs, files in os.walk(home):
        for d in dirs:
            if d == find_dir or d == find_dir + ".git":
                example_basedir = os.path.join(root, d)
                if subdir is not None:
                    example_basedir = os.path.join(example_basedir, subdir)
                break
        if example_basedir is not None:
            example_basedir = os.path.abspath(example_basedir)
            print(f"Example base directory: {example_basedir}")
            break
    return example_basedir


def get_example_dirs(example_basedir, exclude, prefix="test", find_sim=True):
    example_dirs = [
        d
        for d in os.listdir(example_basedir)
        if prefix in d and d not in exclude
    ]

    # make sure mfsim.nam is present in each directory
    if find_sim:
        remove_dirs = []
        # add_dirs = []
        for temp_dir in example_dirs:
            epth = os.path.join(example_basedir, temp_dir)
            fpth = os.path.join(epth, "mfsim.nam")
            if not os.path.isfile(fpth):
                remove_dirs.append(temp_dir)
            # for sub_dir in ("mf6gwf", "mf6gwt"):
            #     tpth = os.path.join(epth, sub_dir)
            #     fpth = os.path.join(tpth, "mfsim.nam")
            #     if os.path.isfile(fpth):
            #         add_dirs.append(os.path.join(temp_dir, sub_dir))

        for remove_dir in remove_dirs:
            example_dirs.remove(remove_dir)

        # example_dirs += add_dirs

    # sort in numerical order for case sensitive os
    example_dirs = sorted(
        example_dirs, key=lambda v: (v.upper(), v[0].islower())
    )

    return example_dirs


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
