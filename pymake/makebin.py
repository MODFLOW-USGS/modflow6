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

pmobj = pymake.Pymake()
pmobj.target = "mf6"
pmobj.appdir = os.path.join("..", "bin")
pmobj.srcdir = os.path.join("..", "src")
pmobj.cc = None
pmobj.subdirs = True
pmobj.inplace = True
pmobj.verbose = True
pmobj.makeclean = True
pmobj.excludefiles = "excludefiles.txt"
pmobj.makefile = False

pmobj.build()
