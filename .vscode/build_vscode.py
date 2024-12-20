import subprocess
import os
import argparse
import platform
import shutil
import shlex

parser = argparse.ArgumentParser()
parser.add_argument("--compiler", type=str)
parser.add_argument("--buildtype", type=str)
parser.add_argument("--pixi", action="store_true")
parser.add_argument("action")
args = parser.parse_args()

os.environ["FC"] = args.compiler
builddir = f"_builddir_{platform.system()}_{args.compiler}_{args.buildtype}"

arg_extended = "-Dextended=false"
if os.getenv("BUILD_EXTENDED_MF6") is not None:
    if os.environ["BUILD_EXTENDED_MF6"] == '1':
        arg_extended = "-Dextended=true"

if args.action == "rebuild" and os.path.isdir(builddir):
    shutil.rmtree(builddir)

if args.buildtype == "release":
    setup_flag = ["-Doptimization=2"]
elif args.buildtype == "debug":
    setup_flag = ["-Ddebug=true", "-Doptimization=0"]

if not os.path.isdir(builddir):
    if args.pixi:
        command = [
            "pixi",
            "run",
            "setup",
        ]
    else:
        command = [
            "meson",
            "setup",
        ]
    command += [
        builddir,
        "--prefix",
        os.getcwd(),
        "--libdir",
        "bin",
        arg_extended,
    ] + setup_flag
    print("Run:", shlex.join(command))
    subprocess.run(
        command,
        check=True,
    )

# Remove all files from bin folder
bin_dir = os.path.join(os.getcwd(), "bin")
if os.path.isdir(bin_dir):
    for dir_entry in os.scandir(bin_dir):
        path = dir_entry.path
        if os.path.isfile(path):
            os.remove(path)

if args.pixi:
    command = ["pixi", "run", "build",]
else:
    command = ["meson", "install", "-C"]    
command += [builddir]
print("Run:", shlex.join(command))
subprocess.run(command, check=True)
