# Building and Testing MODFLOW 6

This document describes how to set up your development environment to build and test MODFLOW 6.
It also explains the basic mechanics of using `git`.

* [Prerequisite Software](#prerequisite-software)
* [Getting the Sources](#getting-the-sources)
* [Installing NPM Modules](#installing-npm-modules)
* [Building](#building)
* [Running Tests Locally](#running-tests-locally)

See the [contribution guidelines](https://github.com/MODFLOW-USGS/modflow6/blob/develop/CONTRIBUTING.md)
if you'd like to contribute to MODFLOW 6.

## Prerequisite Software

Before you can build and test MODFLOW 6, you must install and configure the
following products on your development machine.

### Git

[Git](https://git-scm.com) and/or the **GitHub app** (for [Mac](https://mac.github.com) or [Windows](https://windows.github.com)).
[GitHub's Guide to Installing Git](https://help.github.com/articles/set-up-git) is a good source of information.

### Meson

Meson is used to build MODFLOW 6.
Install Meson and assure it is in your PATH: https://mesonbuild.com/Getting-meson.html


### gfortran (version 4.9 to 8)

gfortran can be used to compile MODFLOW 6 and associated utilities and generate distributable files.

#### Linux

- fedora-based: `dnf install gcc-gfortran`
- debian-based: `apt install gfortran`

#### macOS

- Install [homebrew](https://brew.sh/)
- `brew install gcc`

#### Windows

- Download the Minimalist GNU for Windows (MinGW) installer from Source Forge:
  https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/installer/mingw-w64-install.exe
- Run the installer. Make sure to change `Architecture` to `x86_64`. Leave the
  other settings on default.
- Find the `mingw64/bin` directory in the installation and add it
  to your PATH. Find `Edit the system environment variables` in your Windows
  Start Screen. Click the `Environmental Variables` button and double-click the
  `Path` variable in the User Variables (the top table). Click the `New` button
  and enter the location of the `mingw64/bin` directory.


### Python

Install Python, for example via miniconda: https://docs.conda.io/en/latest/miniconda.html
Then install all packages necessary to run the tests either by executing [install-python-std.sh](.github/common/install-python-std.sh) via bash directly or by installing the listed packages manually.

### ifort (optional)

Intel fortran can be used to compile MODFLOW 6 and associated utilities and generate distributable files (if not using gfortran).
Download the Intel oneAPI HPC Toolkit: https://software.intel.com/content/www/us/en/develop/tools/oneapi/hpc-toolkit/download.html

### LaTeX (optional)
[LaTeX](https://www.latex-project.org/) which is used to generate the MODFLOW 6 Input/Output document (docs/mf6io/mf6io.nightlybuild).

## Getting the Sources

Fork and clone the MODFLOW 6 repository:

1. Login to your GitHub account or create one by following the instructions given
   [here](https://github.com/signup/free).
2. [Fork](http://help.github.com/forking) the [main MODFLOW 6](https://github.com/MODFLOW-USGS/modflow6).
3. Clone your fork of the MODFLOW 6 repository and define an `upstream` remote pointing back to the MODFLOW 6 repository that you forked in the first place.

```shell
# Clone your GitHub repository:
git clone git@github.com:<github username>/modflow6.git

# Go to the MODFLOW 6 directory:
cd modflow6

# Add the main MODFLOW 6 repository as an upstream remote to your repository:
git remote add upstream https://github.com/MODFLOW-USGS/modflow6.git
```

## Building

You can build modflow with Visual Studio Code tasks as described [here](.vscode/README.md).
For the more general instructions, continue to read this section.

First configure the build directory:

```shell
# bash (linux and macOS)
meson builddir --prefix=$(pwd) --libdir=bin

# cmd (windows)
meson builddir --prefix=%CD% --libdir=bin
```

For the following compilations, only this command has to be executed:

```shell
meson compile -C builddir
```

In order to run the tests the binaries have to be installed by executing:

```shell
meson install -C builddir
```

The binaries can then be found in the `bin` folder.


## Running Tests Locally

For complete testing as done on the CI, clone the modflow6-testmodels repository:

```shell
# Clone your GitHub repository:
git clone git@github.com:<github username>/modflow6-testmodels.git
```
* The modflow6-testmodels repository must be cloned in the same directory that contains the modflow6 repository.

To run tests first change directory to the `autotest` folder:

```shell
cd modflow6/autotest
```

Update your flopy installation by executing

```shell
python update_flopy.py
```

Then download latest release binaries and build latest release by executing:

```shell
pytest -v get_exes.py
```

Then the tests can be run with commands similar to these:

```shell
# Build MODFLOW 6 tests generated using flopy
pytest -v test_*

# Build MODFLOW 6 example tests
pytest -v test_z01_testmodels_mf6.py

# Build MODFLOW 5 to 6 converter example tests
pytest -v test_z02_testmodels_mf5to6.py
```

You should execute the test suites before submitting a PR to github.


All the tests are executed on our Continuous Integration infrastructure and a PR could only be merged once the tests pass.

- Github Actions CI fails if any of the test suites described above fails.
