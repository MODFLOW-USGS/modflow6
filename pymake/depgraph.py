#! /usr/bin/env python
import os

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    print(msg)
    raise Exception()
import shutil

srcpth = os.path.join("..", "src")
networkx = False
deppth = "dependencies"
if not networkx:
    deppth += "_std"
if os.path.exists(deppth):
    shutil.rmtree(deppth)
os.makedirs(deppth)

pymake.make_plots(srcpth, deppth, include_subdir=True, networkx=networkx)
