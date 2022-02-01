#!/usr/bin/python

"""
mk_folder_struct.py: Create a latex file containing structure of a folder.

This script is used to update folder_struct.tex using the structure of a specified
distribution folder.  The distribution folder must be specified using a command line
argument.  Usage is:

  python mk_folder_struct.py -dp <distribution path>

"""


import os
import sys


def list_files(startpath, includefiles=False):
    s = ''
    for root, dirs, files in os.walk(startpath):
        level = root.replace(startpath, '').count(os.sep)
        indent = ' ' * 4 * (level)
        s += '{}{}/ \n'.format(indent, os.path.basename(root))
        subindent = ' ' * 4 * (level + 1)
        for f in files:
            if includefiles:
                s += '{}{} \n'.format(subindent, f)
    return s


def get_distribution_path():
    distribution_path = None
    for idx, arg in enumerate(sys.argv):
        if arg == "-dp":
            distribution_path = sys.argv[idx + 1]
    return distribution_path


distribution_path = get_distribution_path()
if distribution_path is None:
    raise Exception(f"Distribution path must be set from command line using -dp <dp>")
if not os.path.isdir(distribution_path):
    raise Exception(f"Distribution path not found: {distribution_path}")

# Open the file
f = open('folder_struct.tex', 'w')
    
# Write the latex header
s = r'''\begin{verbatim}'''
f.write(s)
f.write('\n')

s = list_files(distribution_path)
f.write(s)

s = r'''\end{verbatim}
'''
f.write(s)

f.close()

print('done...')
