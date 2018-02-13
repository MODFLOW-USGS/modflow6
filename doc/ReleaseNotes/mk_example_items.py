
import os
import sys


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


# Set up the path to the distribution
fname = os.path.join('..', 'version.tex')
dist = get_distribution_name(fname)
distpth = os.path.join('..', '..', 'distribution', dist)
if not os.path.isdir(distpth):
    raise Exception(distpth + ' does not exist. ')

# Open the file
f = open('example_items.tex', 'w')
    
# Write the latex header

s = r'''\begin{itemize}'''
f.write(s)
f.write('\n')

expth = os.path.join(distpth, 'examples')
files = os.listdir(expth)
for exname in files:
    
    # Skip if not a directory
    if not os.path.isdir(os.path.join(expth, exname)):
        continue
    
    # example name
    s = r'\item {}---'.format(exname)
    
    line = 'XXX'
    
    descriptionfile = os.path.join(expth, exname, 'description.txt')
    if os.path.isfile(descriptionfile):
        with open(descriptionfile, 'r') as fd:
            line = fd.readline()
        fd.close()
    
    s += line
    f.write(s)
    f.write('\n')
    

s = r'''\end{itemize}
'''
f.write(s)

f.close()

print('done...')
