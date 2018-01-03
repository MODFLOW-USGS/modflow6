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

extrafiles = os.path.join('extrafiles.txt')
args.extrafiles = extrafiles

pymake.pymake.main(args.srcdir, args.target, args.fc, args.cc, args.makeclean,
                   args.expedite, args.dryrun, args.double, args.debug, 
                   args.subdirs, args.fflags, args.arch, args.makefile,
                   args.commonsrc, args.extrafiles)
