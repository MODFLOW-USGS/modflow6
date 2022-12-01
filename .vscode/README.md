# Compiling and Visual Debugging with VSCode

VSCode is a free integrated development environment for Windows, Linux, and
MacOS. There are two major advantages over Visual Studio:

* Most of VSCode's source code is open-source and the releases are (proprietary) freeware.
* VSCode runs on Linux.

This folder contains the configuration files for using VSCode for MODFLOW 6 
development.
At the moment they are used for building and debugging MODFLOW 6.
In order to build MODFLOW 6 follow the steps in [DEVELOPER.md](../DEVELOPER.md)

## Visual Studio Code

Install VSCode from https://code.visualstudio.com/

### Extensions

Install the following VSCode extensions:

- Modern Fortran:
  https://marketplace.visualstudio.com/items?itemName=krvajalm.linter-gfortran

Note: The Remote - WSL extension may be required if you want to use a windows VSCode
installation in a WSL environment.

### Dependencies

Required and optional dependencies for MODFLOW 6 are discussed in [DEVELOPER.md](../DEVELOPER.md)

### Settings

Add [settings.json](https://code.visualstudio.com/docs/getstarted/settings#_settingsjson) to the
`modflow6/.vscode` directory if not already there. The following settings can be considered a
starting place as the contents of this file are dictated both by desired VSCode behavior and
environmental factors:

In general, to determine the path of an installed tool in your environment run:
- bash: `which <toolname>`, e.g. `which fortls`
- cmd: `where <toolname>`, e.g. `where python`
- PowerShell: `Get-Command <toolname>` e.g. `Get-Command fprettify`

1. Activate the conda environment:

```bash
conda activate modflow6
```

2. Determine the path of `fortls` and `fprettify`

3. Set the setting "fortran.fortls.path" and "fortran.formatting.path":
```json
{
    "fortran.fortls.path": "/path/to/fortls",
    "fortran.formatting.path": "/path/to/fprettify",
}
```

The fortran formatter can be integrated with VSCode using the following settings:

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

### Running VSCode

Open the top level `modflow6` repository directory on your system when starting VSCode. The program will
then look for the `modflow6/.vscode` directory to discover settings relevant to your session.

A nice alternative on any system is to start VSCode from the shell. For example, in a bash or git bash
shell (windows), change to the `modflow6` directory and execute the command:

```bash
code .
```

Note the dot. Starting in this way, VSCode will open as desired, inheriting and discovering
expected runtime settings in the correct directory location.

### Compiling

In order to compile Fortran source run:

* Press `Ctrl + Shift + P` in VSCode.
* Type `Tasks`.
* Select `Run Task` (press Enter).
* Select the suitable task for your situation.

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
