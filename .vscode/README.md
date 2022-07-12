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

- vscode-modern-fortran-formatter:
  https://marketplace.visualstudio.com/items?itemName=yukiuuh.vscode-modern-fortran-formatter

- Remote - WSL:
  https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl

Remote - WSL is only needed if you want to use a windows vscode installation in a WSL environment.

### Dependencies

Required and optional dependencies for modflow6 are discussed in [DEVELOPER.md](../DEVELOPER.md)

The Modern Fortran extension requires a Fortran compiler and language server.  To install
the [fortls](https://github.com/gnikit/fortls) fortran language server run:

```bash
pip install -U fortls
```

The vscode-modern-fortran-formatter requires `fprettify`, which can be installed in a similar way
using pip or conda.


### Settings

Add [settings.json](https://code.visualstudio.com/docs/getstarted/settings#_settingsjson) to the
`modflow6/.vscode` directory if not already there. The following settings can be considered a
starting place as the contents of this file are dictated both by desired VSCode behavior and
environmental factors:

In general, to determine the path of an installed tool in your environment run:
- bash: `which <toolname>`, e.g. `which fortls`
- cmd: `where <toolname>`, e.g. `where python`
- PowerShell: `Get-Command <toolname>` e.g. `Get-Command fprettify`

The setting "python.defaultInterpreterPath" points toward your Python executable:
```json
{
    "python.defaultInterpreterPath": "/path/to/python",
}
```

The setting "fortran.fortls.path" points toward your fortls executable:
```json
{
    "fortran.fortls.path": "/path/to/fortls"
}
```

The fortran formatter can be integrated with vscode using the following settings:

```json
{
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "yukiuuh.vscode-modern-fortran-formatter",
    "modernFortranFormatter.fprettifyArgs": "-c C:\\<full path to modflow6>\\distribution\\.fprettify.yaml"
}
```

...or, in a WSL environment:

```json
{
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "yukiuuh.vscode-modern-fortran-formatter",
    "modernFortranFormatter.fprettifyArgs": "-c /<full path to modflow6>/distribution/.fprettify.yaml",
}
```

Setting the formatter up in this way will cause a source file to reformat with each explicit save.

### Running VSCode

Open the top level `modflow6` repository directoy on your system when starting VSCode. The program will
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

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Launch Modflow Model",
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
