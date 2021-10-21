import subprocess
import os
import argparse
import shutil

parser = argparse.ArgumentParser()
parser.add_argument("--compiler", type=str)
parser.add_argument("action")
args = parser.parse_args()

os.environ["FC"] = args.compiler
builddir = f"builddir_{args.compiler}"


if args.action == "rebuild":
    if os.path.isdir(builddir):
        shutil.rmtree(builddir)

    subprocess.run(
        ["meson", "setup", builddir, "--prefix", os.getcwd(), "--libdir", "bin"],
        check=True,
    )

subprocess.run(["meson", "compile", "-C", builddir], check=True)

subprocess.run(["meson", "install", "-C", builddir], check=True)
