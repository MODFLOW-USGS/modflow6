import os
import sys
import flopy


def target_pth(target, pth):
    exe_exists = flopy.which(target)
    if exe_exists is None:
        target = os.path.abspath(os.path.join(pth, target))
    else:
        target = os.path.abspath(exe_exists)
    return target


target_ext = ""
target_so = ".so"
sysinfo = sys.platform.lower()
if sysinfo.lower() == "win32":
    target_ext = ".exe"
    target_so = ".dll"

# paths to executables for  previous versions of MODFLOW
ebindir = os.path.join(os.path.expanduser("~"), ".local", "bin")

# paths to MODFLOW 6 executable, source files, and example files
bindir = os.path.join("..", "bin")

# create dictionary of valid executable targets for regression tests
target_dict = {}

target = target_pth("mf2005dbl{}".format(target_ext), ebindir)
target_dict["mf2005"] = target
target = target_pth("mfnwtdbl{}".format(target_ext), ebindir)
target_dict["mfnwt"] = target
target = target_pth("mfusgdbl{}".format(target_ext), ebindir)
target_dict["mfusg"] = target
target = target_pth("mflgrdbl{}".format(target_ext), ebindir)
target_dict["mflgr"] = target
target = target_pth("mf2005{}".format(target_ext), ebindir)
target_dict["mf2005s"] = target
target = target_pth("mt3dms{}".format(target_ext), ebindir)
target_dict["mt3dms"] = target
target = target_pth("mf6{}".format(target_ext), ebindir)
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
