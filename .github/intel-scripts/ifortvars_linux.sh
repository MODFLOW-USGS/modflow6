#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

#shellcheck disable=SC2010
LATEST_VERSION=$(ls -1 /opt/intel/oneapi/compiler/ | grep -v latest | sort | tail -1)
# shellcheck source=/dev/null
source /opt/intel/oneapi/compiler/"$LATEST_VERSION"/env/vars.sh

cd .github/intel-scripts
ifort hello.f90 -o hello.out
ls
./hello.out
