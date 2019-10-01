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
args = pymake.pymake.parser()

args.subdirs = True

pymake.pymake.main(args.srcdir, args.target, 
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
                   syslibs=args.syslibs,
                   arch=args.arch, 
                   makefile=args.makefile, 
                   srcdir2=None, 
                   extrafiles=None)
