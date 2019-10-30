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

excludefiles = os.path.join('excludefiles.txt')
args.excludefiles = excludefiles

args

pymake.pymake.main(args.srcdir, args.target, fc=args.fc, cc=args.cc,
                   makeclean=args.makeclean, expedite=args.expedite,
                   dryrun=args.dryrun, double=args.double,
                   debug=args.debug, include_subdirs=args.subdirs,
                   fflags=args.fflags, arch=args.arch,
                   makefile=args.makefile, srcdir2=args.commonsrc,
                   extrafiles=args.extrafiles,
                   excludefiles=args.excludefiles)
