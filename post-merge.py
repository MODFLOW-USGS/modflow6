#!/usr/bin/python

from __future__ import print_function
import subprocess
import os

from hook_files import paths, files

def unstage_files():
    for p, f in zip(paths, files):
        if p == '.':
            fpth = f
        else:
            fpth = os.path.join(p, f)
        
        # reset select files
        cargs = ['git', 'reset', 'HEAD', fpth]
        try:
            # add modified version file
            print('Resetting...{}'.format(fpth))
            b = subprocess.Popen(cargs,
                                 stdout=subprocess.PIPE).communicate()[0]
        except:
            print('Could not reset...{}'.format(fpth))
            sys.exit(1)
        
        # checkout existing files
        cargs = ['git', 'checkout', fpth]
        try:
            # add modified version file
            print('Checking out existing...{}'.format(fpth))
            b = subprocess.Popen(cargs,
                                 stdout=subprocess.PIPE).communicate()[0]
        except:
            print('Could not check out existing...{}'.format(fpth))
            sys.exit(1)
    return

if __name__ == "__main__":
    unstage_files()
