#!/bin/bash
set -e

sudo apt-add-repository -y ppa:ubuntu-toolchain-r/test
sudo apt-get update -y
sudo apt-get install -y $FC
if [ "${FC}" = "gfortran-8" ]; then
  echo "installing latex"
  sudo apt-get install -y texlive texlive-latex-extra
       texlive-latex-recommended texlive-science texlive-fonts-extra
fi
echo "Current PYTHONPATH ${PYTHONPATH}"
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O $HOME/miniconda.sh
bash $HOME/miniconda.sh -b -p $HOME/anaconda
export PATH="$HOME/anaconda/bin:$PATH"
export PYTHONPATH="$HOME/anaconda/bin"
echo "Revised PYTHONPATH ${PYTHONPATH}"
hash -r
conda config --set always_yes yes --set changeps1 no
conda update -q conda
conda config --add channels conda-forge
conda install --file requirements.travis.txt
conda info -a

