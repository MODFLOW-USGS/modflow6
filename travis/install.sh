#!/bin/bash
set -e

if [[ ! -d "$HOME/.local/bin" ]]; then mkdir "$HOME/.local/bin"; fi
ln -fs /usr/bin/$FC $HOME/.local/bin/gfortran
gfortran --version
