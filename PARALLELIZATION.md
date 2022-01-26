# Installation Instructions PETSC

## Ubuntu 

- Install Python (e.g. with miniconda)
- `sudo apt install build-essential gfortran git openmpi-bin liblapack-dev`
- `git clone -b release https://gitlab.com/petsc/petsc`
- `cd petsc`
- `./configure`
- `make all check`
