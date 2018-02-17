
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

def get_distribution_name(versiontexname):
    dist = None
    fname = versiontexname
    with open(fname) as f:
        lines = f.readlines()
        f.close()
    for line in lines:
        # \newcommand{\modflowversion}{mf6beta0.9.00}
        srchtxt = '\\newcommand{\\modflowversion}'
        istart = line.rfind('{') + 1
        istop = line.rfind('}')
        if 0 < istart < istop:
            dist = line[istart: istop]
            return dist
    return None


pth = os.path.join('..', '..')
if pth not in sys.path:
    sys.path.append(pth)

# Find version information
fname = os.path.join('..', 'version.tex')
dist = get_distribution_name(fname)

# Set up the path to the distribution
#dist = 'mf6beta.0.6.00'
distpth = os.path.join('..', '..', 'distribution', dist)
if not os.path.isdir(distpth):
    raise Exception(distpth + ' does not exist. ')

# Open the file
f = open('folder_struct.tex', 'w')
    
# Write the latex header

s = r'''\begin{verbatim}'''
f.write(s)
f.write('\n')

s = list_files(distpth)
f.write(s)

s = r'''\end{verbatim}
'''
f.write(s)

f.close()

print('done...')
