#!/usr/bin/env bash

if [[ "$3" == "ifort" ]];
then
    source /opt/intel/oneapi/setvars.sh
fi

# run python script
pixi run python $1 $2 $3 $4 $5 $6
