#!/bin/bash
set -e

echo "TRAVIS_BRANCH=$TRAVIS_BRANCH, BRANCH=$BRANCH"
pwd
git clone https://github.com/MODFLOW-USGS/modflow6-testmodels ../modflow6-testmodels
cd ../modflow6-testmodels
if git show-ref -q --heads $BRANCH; then
  git checkout $BRANCH; echo switched to modflow6-testmodels branch $BRANCH;
  else echo using modflow6-testmodels branch master;  fi
git branch
cd ../modflow6
ls ../
which python
python --version
python -c "import numpy as np; \
  print('numpy version {}'.format(np.__version__))"
python -c "import flopy; flopypth = flopy.__path__[0]; \
  print('flopy is installed in {}'.format(flopypth))"
python -c "import flopy; dir(flopy.mf6)"
cd ./autotest
python update_flopy.py
cd ..
python -c "import flopy; dir(flopy.mf6)"
which nosetests
nosetests --version
