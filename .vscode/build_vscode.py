import subprocess
import os
import argparse
import shutil
import shlex

parser = argparse.ArgumentParser()
parser.add_argument("--compiler", type=str)
parser.add_argument("--buildtype", type=str)
parser.add_argument("action")
args = parser.parse_args()

os.environ["FC"] = args.compiler
builddir = f"builddir_{args.compiler}_{args.buildtype}"

if args.action == "rebuild" and os.path.isdir(builddir):
    shutil.rmtree(builddir)

if args.buildtype == "release":
    setup_flag = ["-Doptimization=2"]
elif args.buildtype == "debug":
    setup_flag = ["-Doptimization=0"]

if not os.path.isdir(builddir):
    command = [
        "meson",
        "setup",
        builddir,
        "--prefix",
        os.getcwd(),
        "--libdir",
        "bin",
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

command = ["meson", "install", "-C", builddir]
print("Run:", shlex.join(command))
subprocess.run(command, check=True)
