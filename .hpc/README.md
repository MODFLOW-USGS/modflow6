
# Building MODFLOW 6 on HPC systems

_On Denali_ 

```
sbatch --reservation=dev cray-meson-build.slurm.batch
```

_Hovenweep_

```
sbatch --reservation=dev cray-hovenweep-meson-build.slurm.batch
```


## Create a module file for a new version of MODFLOW 6

On _Denali_ make a copy of an existing module file using
```
rsync /home/software/denali/contrib/impd/modulefiles/modflow/6.5.0.dev0 /home/software/denali/contrib/impd/modulefiles/modflow/6.x.x
```
On _Hovenweep_ make a copy of an existing module file using
```
rsync /home/software/hovenweep/contrib/impd/modulefiles/modflow/6.5.0.dev0 /home/software/hovenweep/contrib/impd/modulefiles/modflow/6.x.x
```

Edit `product_version` in the new module file from `6.5.0.dev0` to `6.x.x` on both systems.


## Profiling memory usage

The `sstat_poll.py` script can be used profile the memory usage of a job while it running. It uses the SLURM utility `sstat` to profile a job at a fixed interval (default is 30 sec.). The script can not be run with python 2.7. On both Denali and Hovenweep load the cray-python using module

```
module load cray-python
```

After loading cray-python run the script from any location on Denali or Hovenweep using

```
python sstat_poll.py JobID
```

where `JobID` is the SLURM JobID of the job you want to profile. Additional user controls  can be specified and can be identified using

```
python sstat_poll.py -h
```

Currently available options include

```
usage: sstat_poll [-h] [--format FORMAT] [--output OUTPUT] [--prefix PREFIX] [--command COMMAND] [--interval INTERVAL] jobid

python script for polling a SLURM job while it is running on a fixed interval. The python uses the SLURM command 'sstat' to return information on the job. By default, the script returns JobID, AveCPU, AveRSS, and MaxRSS but other data can be returned by specifying the format argument (--format=JobID,AveCPU,AveRSS,MaxRSS,...).

positional arguments:
  jobid                SLURM JobID

options:
  -h, --help           show this help message and exit
  --format FORMAT      SLURM sstat format string (default is JobID,AveCPU,AveRSS,MaxRSS)
  --output OUTPUT      Output file (default is None)
  --prefix PREFIX      Output file prefix (default is None)
  --command COMMAND    SLURM function (default is sstat)
  --interval INTERVAL  polling interval in sec. (default is 30.0 sec.)
```
