#!/usr/bin/env bash

if [[ "$3" == "ifort" ]];
then
    source /opt/intel/oneapi/setvars.sh
fi

# run python script
python3 $1 $2 $3 $4
