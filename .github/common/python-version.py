import sys
import nose
import numpy as np
import matplotlib as mpl
import flopy
import pymake

flopypth = flopy.__path__[0]
pymakepth = pymake.__path__[0]
print("python version:     {}".format(sys.version))
print("nosetest version:   {}".format(nose.__version__))
print("numpy version:      {}".format(np.__version__))
print("matplotlib version: {}".format(mpl.__version__))
print("flopy version:      {}".format(flopy.__version__))
print("pymake version:     {}".format(pymake.__version__))
print("")
print("flopy is installed in:  {}".format(flopypth))
print("pymake is installed in: {}".format(pymakepth))
