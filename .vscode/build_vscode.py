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

arg_parallel = "-Dparallel=false"
if os.getenv("BUILD_PARALLEL_MF6") is not None:
    if os.environ["BUILD_PARALLEL_MF6"] == '1':
        arg_parallel = "-Dparallel=true"

if args.action == "rebuild" and os.path.isdir(builddir):
    shutil.rmtree(builddir)

if not os.path.isdir(builddir):
    command = ["pixi", "run", f"setup-{args.buildtype}", builddir, arg_parallel]
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

command = ["pixi", "run", "install-build", builddir]
print("Run:", shlex.join(command))
subprocess.run(command, check=True)
