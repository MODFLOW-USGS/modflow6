import os
import sys
import subprocess
import flopy


def target_pth(target, pth):
    exe_exists = flopy.which(target, path=pth)
    # if target does not exist in specified path determine if it
    # exists anywhere in the path
    if exe_exists is None:
        exe_exists = flopy.which(target)
    if exe_exists is None:
        exe_exists = os.path.abspath(os.path.join(pth, target))
        raise Exception(f"{exe_exists} does not exist or is not executable.")
    return os.path.abspath(exe_exists)


target_ext = ""
target_so = ".so"
sysinfo = sys.platform.lower()
if sysinfo.lower() == "win32":
    target_ext = ".exe"
    target_so = ".dll"
elif sysinfo.lower() == "darwin":
    target_so = ".dylib"

# paths to executables for  previous versions of MODFLOW
downloaded_bindir = os.path.join("..", "bin", "downloaded")
rebuilt_bindir = os.path.join("..", "bin", "rebuilt")

# paths to MODFLOW 6 executable, source files, and example files
bindir = os.path.join("..", "bin")

# create dictionary of valid executable targets for regression tests
target_dict = {}

target = target_pth("mf2005dbl{}".format(target_ext), downloaded_bindir)
target_dict["mf2005"] = target
target = target_pth("mfnwtdbl{}".format(target_ext), downloaded_bindir)
target_dict["mfnwt"] = target
target = target_pth("mfusgdbl{}".format(target_ext), downloaded_bindir)
target_dict["mfusg"] = target
target = target_pth("mflgrdbl{}".format(target_ext), downloaded_bindir)
target_dict["mflgr"] = target
target = target_pth("mf2005{}".format(target_ext), downloaded_bindir)
target_dict["mf2005s"] = target
target = target_pth("mt3dms{}".format(target_ext), downloaded_bindir)
target_dict["mt3dms"] = target
target = target_pth("mf6{}".format(target_ext), rebuilt_bindir)
target_dict["mf6-regression"] = target

# create MODFLOW 6 target name and add to dictionary
program = "mf6{}".format(target_ext)
target = os.path.join(bindir, program)
target_dict["mf6"] = target

# create MODFLOW 6 so/dll target name
tprog = "libmf6{}".format(target_so)
ttarg = os.path.join(bindir, tprog)
target_dict["libmf6"] = ttarg

# add MODFLOW 5 to 6 converter to dictionary of valid executable targets
tprog = "mf5to6{}".format(target_ext)
ttarg = os.path.join(bindir, tprog)
target_dict["mf5to6"] = ttarg

# add Zonebudget for 6 to dictionary of valid executable targets
tprog = "zbud6{}".format(target_ext)
ttarg = os.path.join(bindir, tprog)
target_dict["zbud6"] = ttarg


def run_exe(argv, ws="."):
    buff = []
    proc = subprocess.Popen(
        argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=ws
    )
    result, error = proc.communicate()
    if result is not None:
        c = result.decode("utf-8")
        c = c.rstrip("\r\n")
        print("{}".format(c))
        buff.append(c)

    return proc.returncode, buff


def get_mf6_version(version="mf6"):
    """Function to get MODFLOW 6 version number"""
    exe = target_dict[version]
    return_code, buff = run_exe((exe, "-v"))
    if return_code == 0:
        version = buff[0].split()[1]
    else:
        version = None
    return version
