# Building MODFLOW 6 on HPC systems

## SLURM job

## Interactive job

```
module switch PrgEnv-cray/6.0.10 PrgEnv-gnu
module load petsc meson ninja

srun --reservation=dev --account=impd --pty bash

meson setup builddir -Ddebug=false -Dparallel=true --prefix=$(pwd) --libdir=bin
meson install -C builddir
```