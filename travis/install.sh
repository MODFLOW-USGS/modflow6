#!/bin/bash
set -e

echo "PYTHONPATH ${PYTHONPATH}"
if [[ ! -d "$HOME/.local/bin" ]]; then mkdir "$HOME/.local/bin"; fi
ln -fs /usr/bin/$FC $HOME/.local/bin/gfortran
gfortran --version
