#!/bin/bash
set -e

python --version
pip --version
pip config --user set global.progress_bar off
pip install https://github.com/modflowpy/flopy/zipball/develop
pip install https://github.com/modflowpy/pymake/zipball/master
pip install https://github.com/mjr-deltares/modflow6-bmipy/zipball/master

