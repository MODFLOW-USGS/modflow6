import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join("..", "doc")))
from version import __version__
print("Update the Doxyfile with the latest version number")
with open("Doxyfile", "r") as fp:
    lines = fp.readlines()

tag = "PROJECT_NUMBER"
with open("Doxyfile", "w") as fp:
    for line in lines:
        if tag in line:
            line = '{}         = "version {}"\n'.format(tag, __version__)
        fp.write(line)
