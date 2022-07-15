#!/usr/bin/env bash

if [[ "$3" == "ifort" ]];
then
    source /opt/intel/oneapi/setvars.sh
fi

eval "$(conda shell.bash hook)"
conda activate modflow6

# run python script
python $1 $2 $3 $4 $5 $6
