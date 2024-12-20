# Using VSCode with MODFLOW 6

This document describes how to use VSCode to modify, format, build and test MODFLOW 6. VSCode is a free integrated development environment for Windows, Linux, and MacOS. Most of VSCode's source code is open-source and the releases are (proprietary) freeware. 

The folder containing this README markdown file (`.vscode/`) contains the configuration files for using VSCode for MODFLOW 6 development. At the moment they are used for formatting, building, and debugging MODFLOW 6. In order to build MODFLOW 6 follow the steps in [DEVELOPER.md](../DEVELOPER.md)

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Setting up Visual Studio Code](#setting-up-visual-studio-code)
  - [Installation](#installation)
  - [Visual Studio Code Extensions](#visual-studio-code-extensions)
  - [Dependencies](#dependencies)
  - [Running VSCode](#running-vscode)
  - [Final VSCode setup](#final-vscode-setup)
- [MODFLOW 6 VSCode Tasks](#modflow-6-vscode-tasks)
  - [Compiling](#compiling)
  - [Debugging](#debugging)
  - [Formatting](#formatting)
  - [Additional tasks](#additional-tasks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Setting up Visual Studio Code

### Installation

Install VSCode from https://code.visualstudio.com/

### Visual Studio Code Extensions

Install the following VSCode extensions:

- Modern Fortran:
  https://marketplace.visualstudio.com/items?itemName=krvajalm.linter-gfortran

Note: The [Remote - WSL extension](https://code.visualstudio.com/docs/remote/wsl) may be required if you want to use a windows VSCode installation in a WSL environment.

### Dependencies

Required and optional dependencies for MODFLOW 6 are discussed in [DEVELOPER.md](../DEVELOPER.md). Required dependencies should be installed prior to setting up VSCode for MODFLOW 6.

### Running VSCode

Open the top level `modflow6` repository directory on your system when starting VSCode. The program will then look for the `modflow6/.vscode` directory to discover settings relevant to your session.

A nice alternative on any system is to start VSCode from the shell. For example, in a bash or git bash shell (windows), change to the `modflow6` directory and execute the command:

```bash
code .
```

Note the dot (".") at the end of the command. Starting in this way, VSCode will open as desired, inheriting and discovering expected runtime settings in the correct directory location.

### Final VSCode setup

A few additional steps are required to finalize the VSCode setup for MODFLOW 6. The remaining steps are:

1. [Select the python interpreter](https://code.visualstudio.com/docs/python/environments#_working-with-python-interpreters) for the MODFLOW 6 workspace. To select the python interpreter run:

    * Press `Ctrl + Shift + P` in VSCode.
    * Type `Python: Select Interpreter`.
    * Select `Run Task` (press Enter).
    * Select the `modflow6` conda environment.

2. Open a new terminal in VSCode and determine the path to the Fortran language server (`fortls`) and `fprettify` using the syntax for your terminal (`cmd`, `bash`, `powershell`).

3. Set the setting "fortran.fortls.path" and "fortran.formatting.path":

    ```json
    {
        "fortran.fortls.path": "/path/to/fortls",
        "fortran.formatting.path": "/path/to/fprettify",
    }
    ```

4. The fortran formatter can be integrated with VSCode using the following settings:

    ```json
    {
        "[fortran]": {
            "editor.formatOnSave": true,
        },
        "fortran.formatting.formatter": "fprettify",
        "fortran.formatting.fprettifyArgs": ["-c", "/path/to/modflow6/.fprettify.yaml"],
    }
    ```
    
    Setting the formatter up in this way will cause a source file to reformat with each explicit save.

In general, to determine the path to python, the fortran language server (`fortls`), and `fprettify` in your environment run:

- bash, zsh: `which <toolname>`, e.g. `which fortls`
- cmd: `where <toolname>`, e.g. `where python`
- PowerShell: `Get-Command <toolname>` e.g. `Get-Command fprettify`

## MODFLOW 6 VSCode Tasks

A number of MODFLOW 6 development tasks in the [`.vscode/settings.json`](./settings.json) file in the MODFLOW 6 repository. Debugging tasks are included in the `.vscode/launch.json` file, that you will create (instructions below). The tasks includes tasks to compile, debug, and format files. A number of miscellaneous tasks are also included to help prepare required files for Pull Requests (for example, makefiles).

### Compiling

In order to compile Fortran source run:

* Press `Ctrl + Shift + P` in VSCode.
* Type `Tasks: Run Build Task` (press Enter).
* Select the suitable task for your situation.

Tasks are available to build MODFLOW 6 using gfortran and Intel Fortran (ifort). If you have used one of the Rebuild tasks, Build tasks for the same compiler can be used to just recompile modified source files and relink the object files. There are also pixi tasks for MODFLOW 6 build using gfortran.

Available compiling tasks include:

* `Rebuild mf6 (gfortran, release)`
* `Rebuild mf6 (ifort, release)`
* `Rebuild mf6 (gfortran, debug)`
* `Rebuild mf6 (ifort, debug)`
* `Build mf6 (gfortran, release)`
* `Build mf6 (ifort, release)`
* `Build mf6 (gfortran, debug)`
* `Build mf6 (ifort, debug)`
* `Pixi - Rebuild mf6 (gfortran, release)`
* `Pixi - Rebuild mf6 (gfortran, debug)`
* `Pixi - Build mf6 (gfortran, release)`
* `Pixi - Build mf6 (gfortran, debug)`


### Debugging

Add a `launch.json` in `.vscode` similar to this.
Most of the time you will want to debug with gdb.
Only when compiling with ifort on Windows, vsdbg is the preferred debugger.

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Debug (gdb)",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/bin/mf6.exe",
            "args": [],
            "stopAtEntry": false,
            "cwd": "/path/to/modflow6/model",
            "environment": [],
            "externalConsole": false,
            "MIMode": "gdb",
            "miDebuggerPath": "/path/to/gdb.exe",
            "setupCommands": [
                {
                    "description": "Enable pretty-printing for gdb",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                }
            ]
        }
        {
            "name": "Debug (vsdbg)",
            "type": "cppvsdbg",
            "request": "launch",
            "program": "${workspaceFolder}/bin/mf6.exe",
            "args": [],
            "stopAtEntry": false,
            "cwd": "/path/to/modflow6/model",
            "environment": [],
            "console": "integratedTerminal"
        }
    ]
}
````

Also remember to adapt the following two lines in `launch.json` according to your setup:

```json
"cwd": "/path/to/your/modflow6/model",
```

```json
"miDebuggerPath": "/path/to/gdb",
```

After building modflow, you can start debugging.

- Set a breakpoint somewhere in the source code.
- Press `Ctrl + F5` to start debugging.

### Formatting

There are a number of VSCode tasks to determine if any of the Fortran source files, python code or scripts, LaTeX files, and markdown files in the MODFLOW 6 repository need to be reformatted. 

In order format file run:

* Press `Ctrl + Shift + P` in VSCode.
* Type `Tasks: Run Build Task` (press Enter).
* Select the suitable task for your situation.

Currently available formatting tasks include:

* `Check spelling` - check spelling in Fortran source files, python code or scripts, LaTeX files, and markdown files
* `Check Fortran format` - check the format of Fortran source files using `fprettify`
* `Check python format` - check the python formatting using `ruff`
* `Check python lint` - check python files for programmatic and stylistic errors using `ruff`

The Fortran format task is only needed if the fortran formatter has not been [integrated with VSCode](#settings).

### Additional tasks

A number of additional VSCode tasks are available to perform common tasks that are required before making a Pull Request (see the [Developer documents](../DEVELOPER.md)). 

In order run additional tasks run:

* Press `Ctrl + Shift + P` in VSCode.
* Type `Tasks: Run Build Task` (press Enter).
* Select the suitable task for your situation.

Currently available additional tasks include:

* `Run update_flopy.py` - updates FloPy MODFLOW 6 classes in the modflow6 Conda environment using current MODFLOW 6 definition files.
* `Run dfn2f90.py` - updates Fortran definitions using current MODFLOW 6 definition files.
* `Run mf6ivar.py` - Regenerates LaTeX file for the input output guide current MODFLOW 6 definition files.
* `Rebuild makefiles` - Rebuilds the GNU makefile for MODFLOW 6 (`mf6`), ZONEBUDGET 6 (`zbud6`), and the MODFLOW Converter (`mf5to6`).