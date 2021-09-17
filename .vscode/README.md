# Compiling and Visual Debugging with VSCode

VSCode is a free integrated development environment for Windows, Linux, and
MacOS. There are two major advantages over Visual Studio:

* VSCode is free.
* VSCode runs on Linux.

## Brief instructions

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

## Extensive instructions (Windows 10)

Install VSCode, compiler, and debugger:

* Install VSCode from: https://code.visualstudio.com/
* Down the Minimalist GNU for Windows (MinGW) installer from Source Forge:
  https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/installer/mingw-w64-install.exe
* Run the installer. Make sure to change `Architecture` to `x86_64`. Leave the
  other settings on default.
* Optionally, find the `mingw64/bin` directory in the installation and add it
  to your PATH. Find `Edit the system environment variables` in your Windows
  Start Screen. Click the `Environmental Variables` button and double-click the
  `Path` variable in the User Variables (the top table). Click the `New` button
  and enter the location of the `mingw64/bin` directory.

Install the following VSCode extensions:

* Modern Fortran:
  https://marketplace.visualstudio.com/items?itemName=krvajalm.linter-gfortran
* C/C++: https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools
* Fortran Breakpoint Support:
  https://marketplace.visualstudio.com/items?itemName=ekibun.fortranbreaker 
  
Install a conda Python installation if you do not have one yet:

* Grab a miniconda installer from: https://docs.conda.io/en/latest/miniconda.html
* Optionally, tick the checkbox for adding conda to your PATH.

Now, configure the VSCode files for the modflow6 directory. Open the `modflow6`
directory in VSCode. Find `run_python.cmd` in the `.vscode` directory, and change
the following line:

```
call /path/to/your/conda.bat activate your_conda_env
```

If conda has been added to your path and your base environment Python is
modern, the following suffices: 

```
call conda.bat activate base
```

To debug, change the following two lines in `launch.json`:

```json
"cwd": "/path/to/your/modflow6/model",
```

```json
"miDebuggerPath": "/path/to/your/gdb.exe",
```

If you've added the `mingw64/bin` directory to your PATH, the following suffices:

```json
"miDebuggerPath": "gdb.exe"
```

To debug, compile a debug version first:

* Press `Cntrl + Shift + P` in VSCode.
* Type `Tasks`.
* Select `Run Tasks` (press Enter).
* Select `Build MF6 (Debug)`.

Now everything is ready to go.

* Set a breakpoint somewhere in the source code.
* Press `Cntrl + F5` to start debugging.
