#!/bin/bash
set -e

echo "TRAVIS_BRANCH=$TRAVIS_BRANCH, PR=$PR, BRANCH=$BRANCH"
pwd
git clone https://github.com/MODFLOW-USGS/modflow6-examples ../modflow6-examples
cd ../modflow6-examples
if git show-ref -q --heads $BRANCH; then
  git checkout $BRANCH; echo switched to modflow6-examples branch $BRANCH;
  else echo using modflow6-examples branch master;  fi
git branch
cd ../modflow6
ls ../
which python
python --version
python -c "import numpy as np; \
  print('numpy version {}'.format(np.__version__))"
python -c "import flopy; flopypth = flopy.__path__[0]; print('flopy is installed in {}'.format(flopypth))"
python -c "import flopy; dir(flopy.mf6)"
cd ./autotest
python update_flopy.py
cd ..
python -c "import flopy; dir(flopy.mf6)"
which nosetests
nosetests --version
