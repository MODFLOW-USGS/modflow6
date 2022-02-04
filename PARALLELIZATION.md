# Installation Instructions PETSC

## Ubuntu 

- `sudo apt install build-essential gfortran git openmpi-bin liblapack-dev pkg-config`
- `git clone -b release https://gitlab.com/petsc/petsc`
- `cd petsc`
- `./configure` (when building release build also `--with-debugging=no`)
- `make all check`
- `sudo cp arch-linux-c-debug/lib/pkgconfig/PETSc.pc /usr/lib/pkgconfig/`
- add the following e.g. to `~/.bash_profile`: `export LD_LIBRARY_PATH="$(pwd)/arch-linux-c-debug/lib:$LD_LIBRARY_PATH"`
