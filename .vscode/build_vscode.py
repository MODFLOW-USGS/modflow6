import subprocess
import os
import argparse
import platform
import shutil
import shlex

parser = argparse.ArgumentParser()
parser.add_argument("--compiler", type=str)
parser.add_argument("--buildtype", type=str)
parser.add_argument("action")
args = parser.parse_args()

os.environ["FC"] = args.compiler
builddir = f"builddir_{platform.system()}_{args.compiler}_{args.buildtype}"

arg_parallel = "-Dparallel=false"
if os.getenv("BUILD_PARALLEL_MF6") is not None:
    if os.environ["BUILD_PARALLEL_MF6"] == '1':
        arg_parallel = "-Dparallel=true"

setup_flag = [f"-Dbuildtype={args.buildtype}"]
    
if args.action == "rebuild":
    setup_flag += ["--wipe"]

if args.action == "rebuild":
    shutil.rmtree(builddir)
    command = [
        "meson",
        "setup",
        builddir,
        "--prefix",
        os.getcwd(),
        "--libdir",
        "bin",
        arg_parallel,
    ] + setup_flag
    print("Run:", shlex.join(command))
    subprocess.run(
        command,
        check=True,
    )

if args.action == "rebuild" or args.action == "build":
    # Remove all files from bin folder
    bin_dir = os.path.join(os.getcwd(), "bin")
    if os.path.isdir(bin_dir):
        for dir_entry in os.scandir(bin_dir):
            path = dir_entry.path
            if os.path.isfile(path):
                os.remove(path)

    command = ["meson", "install", "-C", builddir]
    print("Run:", shlex.join(command))
    subprocess.run(command, check=True)

