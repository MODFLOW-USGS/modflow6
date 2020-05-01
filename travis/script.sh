#!/bin/bash
set -e


if [ "${FC}" = "gfortran-8" ]; then
  echo "building mfio latex document"
  nosetests -v --with-id --with-timer -w ./autotest --tests=build_mfio_tex.py
fi
nosetests -v --with-id --with-timer -w ./autotest
