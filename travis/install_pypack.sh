#!/bin/bash
set -e

python --version
pip --version
pip config --user set global.progress_bar off
# pip install https://github.com/modflowpy/flopy/zipball/develop
git clone https://github.com/modflowpy/flopy --depth 1 --branch=develop ~/flopy
pip install --user -e ~/flopy
pip install https://github.com/modflowpy/pymake/zipball/master
pip install https://github.com/mjr-deltares/modflow6-bmipy/zipball/master

