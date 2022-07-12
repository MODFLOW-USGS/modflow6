#!/bin/bash

pip install wheel
pip install requests appdirs numpy matplotlib pytest pytest-xdist meson!=0.63.0 ninja fprettify
pip install https://github.com/modflowpy/flopy/zipball/develop
pip install https://github.com/modflowpy/pymake/zipball/master
pip install https://github.com/Deltares/xmipy/zipball/develop
pip install https://github.com/MODFLOW-USGS/modflowapi/zipball/develop
