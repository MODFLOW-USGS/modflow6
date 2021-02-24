#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

source /opt/intel/oneapi/setvars.sh

cd .github/intel-scripts
ifort hello.f90 -o hello.out
ls
./hello.out
