# Building MODFLOW 6 on HPC systems

## SLURM job

```
sbatch --reservation=dev cray-meson-build.slurm.batch
```

## Interactive job

```
module switch PrgEnv-${PE_ENV,,} PrgEnv-intel
module load cray-petsc meson ninja
export PKG_CONFIG_PATH=/opt/cray/pe/mpt/7.7.19/gni/mpich-intel/16.0/lib/pkgconfig:/opt/cray/pe/petsc/3.14.5.0/real/INTEL/19.1/x86_skylake/lib/pkgconfig:$PKG_CONFIG_PATH

srun --reservation=dev --account=impd --pty bash

meson setup builddir -Ddebug=false --prefix=$(pwd) --libdir=bin -Dcray=true -Ddebug=false --wipe
meson install -C builddir
```