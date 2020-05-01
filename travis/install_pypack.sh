#!/bin/bash
set -e

python --version
pip --version
pip config --user set global.progress_bar off
#git clone https://github.com/modflowpy/flopy --depth 1 --branch=develop ~/flopy
#pip install --user -e ~/flopy
pip install https://github.com/modflowpy/flopy/zipball/develop
#git clone https://github.com/modflowpy/pymake --depth 1 --branch=master ~/pymake
#pip install --user -e ~/pymake
pip install https://github.com/modflowpy/pymake/zipball/master
#git clone https://github.com/mjr-deltares/modflow6-bmipy.git --depth 1 --branch=master ~/amipy
#pip install --user -e ~/amipy
pip install https://github.com/mjr-deltares/modflow6-bmipy/zipball/master

