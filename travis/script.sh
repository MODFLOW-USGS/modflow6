#!/bin/bash
set -e


if [ "${FC}" = "gfortran-8" ]; then
  echo "building mfio latex document"
  cd autotest/
  nosetests -v --with-id --with-timer build_mfio_tex.py
  cd ../
fi
nosetests -v --with-id --with-timer -w ./autotest
