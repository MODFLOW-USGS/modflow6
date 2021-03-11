#!/bin/bash

conda info
conda install pip requests appdirs nose
pip install nose-timer
# use pip to install numpy and matplotlib because of a windows issue
pip install numpy matplotlib
pip install https://github.com/modflowpy/flopy/zipball/develop
pip install https://github.com/modflowpy/pymake/zipball/master
pip install https://github.com/Deltares/xmipy/zipball/develop


