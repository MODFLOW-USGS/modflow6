This folder contains the configuration files for using VSCode for MODFLOW 6 
development. Currently this is confirmed to work on Windows, with mingw-w64 
for gfortran and gdb.

You'll want to change the following files:

    launch.json

which contains the gdb debug configuration settings. You will need to put 
in the path to your gdb and the working directory of the MODFLOW 6 model 
you are trying to debug. And,

    run_python.cmd
    
where you need to specify the proper conda environment to be able to build the 
code with pymake.

After that, you should be good to go...



