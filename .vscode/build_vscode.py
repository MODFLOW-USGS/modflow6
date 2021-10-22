import subprocess
import os
import argparse
import shutil

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
    subprocess.run(
        ["meson", "setup", builddir, "--prefix", os.getcwd(), "--libdir", "bin"]
        + setup_flag,
        check=True,
    )

subprocess.run(["meson", "compile", "-C", builddir], check=True)

# Remove all files from bin folder
for file in os.scandir(os.path.join(os.getcwd(), "bin")):
    os.remove(file.path)

subprocess.run(["meson", "install", "-C", builddir], check=True)
