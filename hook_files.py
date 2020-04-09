#!/usr/bin/python

import os

# update files and paths so that there are the same number of
# path and file entries in the paths and files list. Enter '.'
# as the path if the file is in the root repository directory 
paths = ['.', 'doc', '.', '.', 
         '.', 'src/Utilities']
files = ['version.txt', 'version.tex', 'README.md', 'DISCLAIMER.md',
         'code.json', 'version.f90']

# check that there are the same number of entries in files and paths
if len(paths) != len(files):
    msg = 'The number of entries in paths ' + \
          '({}) must equal '.format(len(paths)) + \
          'the number of entries in files ({})'.format(len(files))
    assert False, msg
    
if __name__ == "__main__":
    for p, f in zip(paths, files):
        if p == '.':
            fpth = f
        else:
            fpth = os.path.join(p, f)
        print('git hooks are modifying...{}'.format(fpth))
