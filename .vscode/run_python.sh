#!/usr/bin/env bash

if [[ "$4" == "ifort" ]];
then
    source /opt/intel/oneapi/setvars.sh
fi

# run python script
$1 $2 $3 $4 $5 $6 $7
