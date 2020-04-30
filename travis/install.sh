#!/bin/bash
set -e


if [[ ! -d "$HOME/.local/bin" ]]; then mkdir "$HOME/.local/bin"; fi
ln -fs /usr/bin/$FC $HOME/.local/bin/gfortran
gfortran --version
python --version
pip --version
pip config --user set global.progress_bar off
git clone https://github.com/modflowpy/flopy --depth 1 --branch=develop ~/flopy
pip install --user -e ~/flopy
git clone https://github.com/modflowpy/pymake --depth 1 --branch=master ~/pymake
pip install --user -e ~/pymake
git clone https://github.com/mjr-deltares/modflow6-bmipy.git --depth 1 --branch=master ~/amipy
pip install --user -e ~/amipy
