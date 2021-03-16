#!/bin/bash

echo $HOME
pwd
git clone https://github.com/MODFLOW-USGS/modflow6-examples ../modflow6-examples
cd ../modflow6-examples/etc/
pwd
python python ci_build_files.py
cd ../../modflow6
pwd
