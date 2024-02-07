
# Building MODFLOW 6 on HPC systems

_On Denali_ 

```
sbatch --reservation=dev cray-meson-build.slurm.batch
```

_Hovenweep_

```
sbatch cray-hovenweep-meson-build.slurm.batch
```


## Create a module file for a new version of MODFLOW 6

On _Denali_ make a copy of an existing module file using
```
rsync /home/software/denali/contrib/impd/modulefiles/modflow/6.5.0.dev0 /home/software/denali/contrib/impd/modulefiles/modflow/6.x.x
```
On _Hovenweep_ make a copy of an existing module file using
```
rsync /home/software/hovenweep/contrib/impd/modulefiles/modflow/6.5.0.dev0 /home/software/denali/contrib/impd/modulefiles/modflow/6.x.x
```

Edit `product_version` in the new module file from `6.5.0.dev0` to `6.x.x` on both systems.


