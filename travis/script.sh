#!/bin/bash
set -e


echo "PYTHONPATH ${PYTHONPATH}"
if [ "${FC}" = "gfortran-8" ]; then
  echo "building mfio latex document"
  nosetests -v --with-id --with-timer -w ./autotest/build_mfio_tex.py
fi
nosetests -v --with-id --with-timer -w ./autotest
