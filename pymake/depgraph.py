#! /usr/bin/env python
import os
try:
    import pymake
except:
    msg =  'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    print(msg)
    raise Exception()
import os

srcpth = os.path.join('..', 'src')
deppth = 'dependencies'
if not os.path.exists(deppth):
    os.makedirs(deppth)

pymake.visualize.make_plots(srcpth, deppth, include_subdir=True)
