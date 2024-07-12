#! /usr/bin/env python
try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    print(msg)
    raise Exception()
import os

# get the arguments
args = pymake.parser()

args.subdirs = True
extrafiles = os.path.join("extrafiles.txt")
args.extrafiles = extrafiles
args.verbose = True
args.inplace = True
args.appdir = os.path.join("..", "..", "..", "bin")

pymake.main(
    args.srcdir,
    args.target,
    fc=args.fc,
    cc=args.cc,
    makeclean=args.makeclean,
    expedite=args.expedite,
    dryrun=args.dryrun,
    double=args.double,
    debug=args.debug,
    include_subdirs=args.subdirs,
    fflags=args.fflags,
    cflags=args.cflags,
    arch=args.arch,
    syslibs=args.syslibs,
    makefile=args.makefile,
    srcdir2=args.commonsrc,
    extrafiles=args.extrafiles,
    excludefiles=args.excludefiles,
    sharedobject=args.sharedobject,
    appdir=args.appdir,
    verbose=args.verbose,
    inplace=args.inplace,
)

#  Windows:
#  python makebin.py -fc ifort -sd -mc ../src mf5to6.exe
#
#  Mac/Linux:
#  python makebin.py -sd -mc ../src mf5to6
