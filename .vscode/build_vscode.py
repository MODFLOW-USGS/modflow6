import os
import sys

import pymake

def build_mf6(mode, onlytouched):
    
    fc = 'gfortran'
    cc = 'gcc'
    
    srcdir = os.path.join('..', 'src')
    if mode =="-release":
      # release mode options:
      target = '../bin/mf6.exe'    
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                '-Wunused-variable -pedantic -std=f2008')
    else:
      # debug:
      target = '../bin/mf6d.exe'  
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                '-Wunused-variable -pedantic -std=f2008 '
                ' -O0 -g')
    
    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags, makeclean=not onlytouched, 
                expedite=onlytouched, inplace=True)
 
def build_mf6_dll(mode, onlytouched): 
    
    fc = 'gfortran'
    cc = 'gcc'
    
    srcdir = os.path.join('..', 'srcbmi')
    comdir = os.path.join('..', 'src')
    excludefiles = [os.path.join(comdir, 'mf6.f90')]

    if mode == "-release":
      target = os.path.join('..', 'bin', 'libmf6.dll')
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                  '-Wunused-variable -pedantic -std=f2008')
    else:
      target = os.path.join('..', 'bin', 'libmf6d.dll')
      fflags = ('-Wtabs -Wline-truncation -Wunused-label '
                  '-Wunused-variable -pedantic -std=f2008 '
                  '-O0 -g')

    pymake.main(srcdir, target, fc=fc, cc=cc, include_subdirs=True,
                fflags=fflags, srcdir2=comdir, excludefiles=excludefiles,
                sharedobject=True, makeclean=not onlytouched, expedite=onlytouched, inplace=True)
                
 
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
    
    