#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

source /opt/intel/oneapi/setvars.sh

nosetests -v --nocapture --with-id --with-timer -w ./autotest get_build_exes.py
