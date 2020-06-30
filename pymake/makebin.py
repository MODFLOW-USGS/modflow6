#! /usr/bin/env python
try:
    import pymake
except:
    msg =  'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    print(msg)
    raise Exception()
import os

#get the arguments
args = pymake.parser()

print(
    'srcdir=', args.srcdir, '\n',
    'target=', args.target, '\n',
    'fc=', args.fc, '\n',
    'cc=', args.cc, '\n',
    'makeclean=', args.makeclean, '\n',
    'expedite=', args.expedite, '\n',
    'dryrun=', args.dryrun, '\n',
    'double=', args.double, '\n',
    'debug=', args.debug, '\n',
    'include_subdirs=', args.subdirs, '\n',
    'fflags=', args.fflags, '\n',
    'cflags=', args.cflags, '\n',
    'syslibs=', args.syslibs, '\n',
    'arch=', args.arch, '\n',
    'makefile=', args.makefile, '\n',
    'srcdir2=', args.commonsrc, '\n',
    'extrafiles=', args.extrafiles, '\n',
    'excludefiles=', args.excludefiles, '\n',
    'sharedobject=', args.sharedobject, '\n',
)

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
)

#  Windows:
#  python makebin.py -fc ifort -sd -mc ../src mf6.exe
#
#  Mac/Linux:
#  python makebin.py -sd -mc ../src mf6

