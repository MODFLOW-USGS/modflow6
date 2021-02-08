import os
import sys

import pymake

dll_extension = '.so'
exe_extension = ''
if sys.platform.lower() == "win32":
    dll_extension = '.dll'
    exe_extension = '.exe'


def build_mf6(mode, onlytouched):
    
    target = os.path.join('..', 'bin', 'mf6')
    fc = 'gfortran'
    cc = 'gcc'
    srcdir = os.path.join('..', 'src')

    if mode =="-release":
      # release mode options:
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                '-Wunused-variable -pedantic -std=f2008')
    else:
      # debug mode options:
      target += 'd'
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                '-Wunused-variable -pedantic -std=f2008 '
                ' -O0 -g')
    
    target += exe_extension    
    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags, makeclean=not onlytouched, 
                expedite=onlytouched, inplace=True)
 

def build_mf6_dll(mode, onlytouched): 

    target = os.path.join('..', 'bin', 'libmf6')    
    fc = 'gfortran'
    cc = 'gcc'
    srcdir = os.path.join('..', 'srcbmi')
    comdir = os.path.join('..', 'src')
    excludefiles = [os.path.join(comdir, 'mf6.f90')]

    if mode == "-release":
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                  '-Wunused-variable -pedantic -std=f2008')
    else:
      target += 'd'
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                  '-Wunused-variable -pedantic -std=f2008 '
                  '-O0 -g')

    target += dll_extension
    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags, srcdir2=comdir, excludefiles=excludefiles,
                sharedobject=True, makeclean=not onlytouched, expedite=onlytouched, 
                inplace=True)
                
 
def main():

    target = sys.argv[1]
    mode = sys.argv[2]
    only = sys.argv[3]

    if target == "-exe":
      build_mf6(mode, only == "-only")
    else:
      build_mf6_dll(mode, only == "-only")
                
if __name__ == "__main__":
    main()
    
    