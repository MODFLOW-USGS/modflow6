# Compiling and Visual Debugging with VSCode

VSCode is a free integrated development environment for Windows, Linux, and
MacOS. There are two major advantages over Visual Studio:

* Most of VSCode's source code is open-source and the releases are (proprietary) freeware.
* VSCode runs on Linux.

This folder contains the configuration files for using VSCode for MODFLOW 6 
development.
At the moment they are used for building and debugging MODFLOW 6.

## Preparation


- Install VSCode: https://code.visualstudio.com/
- Install Meson and assure it is in your PATH: https://mesonbuild.com/Getting-meson.html
- You also need to either install `intel fortran` or `gfortran` as described below


### Intel fortran

Download the Intel oneAPI HPC Toolkit: https://software.intel.com/content/www/us/en/develop/tools/oneapi/hpc-toolkit/download.html


### gfortran

Install with your package manager

#### Linux

- `apt install gfortran`
- `dnf install gcc-gfortran`

#### macOS

- `brew install gcc`

#### Windows

- Down the Minimalist GNU for Windows (MinGW) installer from Source Forge:
  https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/installer/mingw-w64-install.exe
- Run the installer. Make sure to change `Architecture` to `x86_64`. Leave the
  other settings on default.
- Optionally, find the `mingw64/bin` directory in the installation and add it
  to your PATH. Find `Edit the system environment variables` in your Windows
  Start Screen. Click the `Environmental Variables` button and double-click the
  `Path` variable in the User Variables (the top table). Click the `New` button
  and enter the location of the `mingw64/bin` directory.


### VSCode extensions

Install the following VSCode extensions:

* Modern Fortran:
  https://marketplace.visualstudio.com/items?itemName=krvajalm.linter-gfortran
* C/C++: https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools
* Fortran Breakpoint Support:
  https://marketplace.visualstudio.com/items?itemName=ekibun.fortranbreaker 
  
Install a conda Python installation if you do not have one yet.
Grab a miniconda installer from: https://docs.conda.io/en/latest/miniconda.html

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



## dasfsad

You'll want to change the following files:

    launch.json

which contains the gdb debug configuration settings. You will need to put 
in the path to your gdb and the working directory of the MODFLOW 6 model 
you are trying to debug. And,

    run_python.cmd
    
where you need to specify the proper conda environment to be able to build the 
code with pymake.

After that, you should be good to go...
