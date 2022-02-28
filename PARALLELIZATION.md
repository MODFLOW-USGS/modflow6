q# Installation Instructions PETSC

## Ubuntu 

- `sudo apt install build-essential gfortran git openmpi-bin liblapack-dev pkg-config`
- `git clone -b release https://gitlab.com/petsc/petsc`
- `cd petsc`
- `./configure` (when building release build also `--with-debugging=no`)
- `make all check`
- This will result in a folder `arch-linux-c-debug` for debug and `arch-linux-c-opt` for release builds
- Add the following to `~/.bashrc`
```
export LD_LIBRARY_PATH="/path/to/compiled/petsc/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/path/to/compiled/petsc/lib/pkgconfig:$PKG_CONFIG_PATH"
```
With `/path/to/compiled/petsc` being the absolute path to `arch-linux-c-debug` or `arch-linux-c-opt` depending on which build type you chose before.
