#!/bin/bash
set -e


export PATH="$HOME/anaconda/bin:$PATH"
if [ "${FC}" = "gfortran-8" ]; then
  echo "building mfio latex document"
  nosetests -v --with-id --with-timer -w ./autotest/build_mfio_tex.py
fi
nosetests -v --with-id --with-timer -w ./autotest
