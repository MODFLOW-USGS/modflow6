
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

meson setup builddir -Ddebug=false --prefix=$(pwd) --libdir=bin --bindir=bin -Dcray=true -Ddebug=false --wipe
meson install -C builddir
```

# Installing a new version of MODFLOW 6 on HPC systems


After building the new version (`6.x.x`) of MODFLOW 6 using a SLURM or interactive job you will need to install the new version and create a new module file.

## Install a new version of MODFLOW 6

Create a directory for the new version in `/home/software/denali/contrib/impd/apps/modflow/`

Since MODFLOW 6 is currently only being compiled with INTEL (version 19.1.0.166), make the following subdirectories in the `/home/software/denali/contrib/impd/apps/modflow/6.x.x` directory you just created.

1. `/INTEL/19.1.0.166/bin`
2. `/INTEL/19.1.0.166/lib`

Copy `mf6` and `zbud6` to the `/INTEL/19.1.0.166/bin` and `libmf6.so` to the `/INTEL/19.1.0.166/lib` subdirectories using

```
rsync ../bin/mf6 /home/software/denali/contrib/impd/apps/modflow/6.x.x/INTEL/19.1.0.166/bin/
```

```
rsync ../bin/zbud6 /home/software/denali/contrib/impd/apps/modflow/6.x.x/INTEL/19.1.0.166/bin/
```

```
rsync ../bin/libmf6.so /home/software/denali/contrib/impd/apps/modflow/6.x.x/INTEL/19.1.0.166/lib/
```
## Create a module file for a new version of MODFLOW 6

Make a copy of an existing module file using
```
rsync /home/software/denali/contrib/impd/modulefiles/modflow/6.4.2 /home/software/denali/contrib/impd/modulefiles/modflow/6.x.x
```

Edit `product_version` in the new module file from `6.4.2` to `6.x.x`.


