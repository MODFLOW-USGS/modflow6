import subprocess
import os
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--compiler', type=str)
parser.add_argument('action')
args = parser.parse_args()

os.environ["FC"] = args.compiler


builddir = f"builddir_{args.compiler}"

subprocess.run(["meson", "setup", builddir, "--prefix", os.getcwd(), "--libdir", "bin"], check=True)

if args.action == "build":
    subprocess.run(["meson", "compile", "-C", builddir], check=True)
elif args.action == "rebuild":
    subprocess.run(["meson", "compile", "-C", builddir, "--clean"], check=True)

subprocess.run(
    [
        "meson",
        "install",
        "-C",
        builddir,
    ],
    check=True,
)
