# Building MODFLOW 6 on HPC systems

## SLURM job

## Interactive job

```
module switch PrgEnv-cray/6.0.10 PrgEnv-gnu
module switch gcc/11.2.0 gcc/8.1.0
module load petsc meson ninja
export PKG_CONFIG_PATH=/opt/cray/pe/mpt/7.7.19/gni/mpich-gnu/8.1/lib/pkgconfig:$PKG_CONFIG_PATH

srun --reservation=dev --account=impd --pty bash

meson setup builddir -Ddebug=false --prefix=$(pwd) --libdir=bin -Dcray=true
meson install -C builddir
```