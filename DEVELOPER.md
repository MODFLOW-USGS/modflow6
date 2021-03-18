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
following products on your development machine:

* [Git](https://git-scm.com) and/or the **GitHub app** (for [Mac](https://mac.github.com) or
  [Windows](https://windows.github.com)); [GitHub's Guide to Installing
  Git](https://help.github.com/articles/set-up-git) is a good source of information.

* [gfortran](https://gcc.gnu.org/wiki/GFortran), (version 4.9 to 8) which is used to compile MODFLOW 6 and associated utilities and generate distributable files.

* (Optional) [Intel Fortran](https://software.intel.com/en-us/fortran-compilers) which is used to compile MODFLOW 6 and associated utilities and generate distributable files (if not using gfortran).

* [python](https://www.python.org/) which is used to run MODFLOW 6 autotests (suggest using an Anaconda python distribution).

* [flopy](https://github.com/modflowpy/flopy) which is used to run MODFLOW 6 autotests.

* [pymake](https://github.com/modflowpy/pymake) which is used to run MODFLOW 6 autotests.

* (Optional) [LaTeX](https://www.latex-project.org/) which is used to generate the MODFLOW 6 Input/Output document (docs/mf6io/mf6io.nightlybuild).

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

To build MODFLOW 6 run:

```shell
# Go to the pymake directory
cd modflow6/pymake

# run the modflow6 pymake build script
python makebin.py -mc ../src/ ../bin/mf6
```

* Results are put in the bin folder.

## Running Tests Locally

(Optional) For complete testing as done on Travis, clone the modflow6-testmodels repository:

```shell
# Clone your GitHub repository:
git clone git@github.com:<github username>/modflow6-testmodels.git
```
* The modflow6-testmodels repository must be cloned in the same directory that contains the modflow6 repository.

To run tests:

```shell
# Go to the autotest directory
cd modflow6/autotest

# Run all modflow6 tests (including building executables and the mfio documentation - requires installation of LaTeX
nosetests -v

# Build MODFLOW 6, MODFLOW 6 utilities, and all versions of MODFLOW used in comparison tests
nosetests -v get_build_exes.py

# Build MODFLOW 6 tests generated using flopy
nosetests -v test_*

# Build MODFLOW 6 example tests
nosetests -v test_z01_testmodels_mf6.py

# Build MODFLOW 5 to 6 converter example tests
nosetests -v test_z02_testmodels_mf5to6.py
```

You should execute the test suites before submitting a PR to github.


All the tests are executed on our Continuous Integration infrastructure and a PR could only be merged once the tests pass.

- Travis CI fails if any of the test suites described above fails.

## <a name="clang-format"></a> Formatting your source code

Add some guidelines

## Linting/verifying your source code

Fortran linting?
