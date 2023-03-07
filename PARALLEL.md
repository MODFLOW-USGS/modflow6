# Developing in parallel MODFLOW 6

This document describes how to set up your build environment for developing and testing the parallel version of MODFLOW. It builds on the instructions given in [DEVELOPER.md](DEVELOPER.md) so make sure to read those first.

---
**DISCLAIMER**

*Expectations on platform compatibility* 

The serial version of the MODFLOW 6 program has had no external dependencies and is traditionally available for a variety of platforms (Windows, GNU/linux, macOS) and compatible with the mainstream Fortran compilers (gfortran, ifort). The parallel version has introduced dependencies on third party components, most notably the MPI and PETSc libraries. While the goal is a continued support of the above mentioned configurations, this has become more challenging and can generally not be guaranteed. To assist developers as well as end users who are compiling the code themselves, a list of successfully tested build configurations will be included in this document.

To improve our support, we kindly ask you to share your experience with building and running parallel MODFLOW with a setup that is not in the list.

---

<!-- TODO: insert TOC tags here -->

## Introduction

The design philosophy has been to maintain MODFLOW as a single codebase and have it compile to either a serial version or to a parallel version. The former continues to be a lightweight, highly compatible code which does not require external dependencies other than those provided by the standard compiler libraries. The latter  has two distinct dependencies on 3rd party libraries: MPI and PETSc, described below. The parallel capability is a true extension and the executable will be capable of serial execution in all cases.

## Prerequisites

### MPI

The parallel software uses the Message Passing Interface (MPI) to synchronize data between processes. There are a couple of implementations of the MPI standard available. Their applicability usually depends on the platform that is used:

- Open MPI: https://www.open-mpi.org/
- MPICH: https://www.mpich.org/
- Intel MPI: https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html
- Microsoft MPI: https://learn.microsoft.com/en-us/message-passing-interface/microsoft-mpi

On Linux and macOS your best bet might be to automatically download the MPI implementation upon configuring PETSc with the option flag `--download-openmpi` or `--download-mpich` (see below). Alternatively you can install OpenMPI or MPICH using the package manager of your OS. It general it would be good to check the table below for tested configurations.

In addition to compiling, the MPI toolset is also required to run a parallel simulation. The implementations above all come with `mpiexec` (or `mpiexec.exe`) to start an executable in parallel. 

### PETSc

PETSc, the Portable, Extensible Toolkit for Scientific Computation, pronounced PET-see (/ˈpɛt-siː/), is a suite of data structures and routines for the scalable (parallel) solution of scientific applications modeled by partial differential equations: https://petsc.org/release/

The PETSc library is used by MODFLOW for its parallel linear solver capabilities and the distributed data formats (vectors and matrices) that go along with it. Parallel PETSc uses MPI internally as well, so setting up this library should typically be coordinated with the installation of the MPI library. A lot of obscure things can go wrong if the binaries are not compatible, so in general it is a good strategy to compile MPI, PETSc, and MODFLOW with the same compiler toolchain.

The PETSc website gives details on a large number of configurations, depending on the target platform/OS, and many different ways to configure/make/install the library: https://petsc.org/release/install/. Building on Windows is notoriously volatile and discouraged by the development team. On Linux, however, it could be as easy as

```
$ ./configure --download-openmpi --download-fblaslapack
$ make all
```

### pkg-config and setting the `PKG_CONFIG_PATH` variable
Eventually, the MODFLOW build process has to resolve the installation location of all external dependencies. The pkg-config tool (https://en.wikipedia.org/wiki/Pkg-config) is used to take care of that. For PETSc, you can check the contents of the folder
```
$PETSC_DIR/$PETSC_ARCH/lib/pkgconfig/
```
and confirm that there are one or more `*.pc` files in there. A similar `pkgconfig/` folder has to present for the MPI installation that was used. For example, for Open MPI on WSL2 this folder is `/lib/x86_64-linux-gnu/pkgconfig/`.

To connect everything, both of these folders have to be added to the `PKG_CONFIG_PATH` variable.

## Building
- meson
- msvs
- makefile
- pymake

## Testing parallel

- markers

## Debugging

The most straightforward way to debug a parallel simulation is to start a run and have it pause to attach the debugger(s). In parallel mode the program uses the PETSc solver and a configuration file `.petscrc` should be present in the same folder as the simulation's `mfsim.nam`. In that PETSc resource file, you can add the following option:

```
-wait_dbg
```
telling MODFLOW to pause immediately after startup. This will give you time to attach one or multiple debuggers to the process. Then start the parallel program, for example on two processor cores:

```
mpiexec -np 2 mf6 -p
```
In the process explorer you should now see 2 processes called `mf6`. On the prompt where the command was executed, MODFLOW waits for input:

```
$ Hit enter to continue...
```

In order to truly debug in parallel, i.e. step through the instructions side-by-side, you will need to start two instances of the debugger and attach them. The following paragraph describes how to do that with VSCode.

### Debugging with VSCode

In VSCode parallel debugging is easiest done by duplicating the development environment. First, make sure that you have set up your environment to build a parallel version in debug mode. The VSCode launch.json should contain an entry to attach to a running process:

```
  {
    "name": "Attach to ...",
    "type": "cppdbg",
    "request": "attach",
    "processId": "${command:pickProcess}",
    "program": "<path_to>/mf6",
    "MIMode": "gdb",
    "miDebuggerPath": "<path_to>/gdb",
    "setupCommands": [
      {
        "description": "Enable pretty-printing for gdb",
        "text": "-enable-pretty-printing",
        "ignoreFailures": true
      }
    ]
  }
```
After building parallel MODFLOW, press `Ctrl+Shift+p` to execute *Workspaces: Duplicate As Workspace in New Window*. This will open a second VSCode window, identical to the first. Starting the debug process and selecting *"Attach to ..."* pop ups a process selection window with the processes started from the `mpiexec` command described above. Select both, each from their own instance of the VSCode program. Now you can put breakpoints in the code and step through the parallel processes side-by-side.

---
**TIP**

Make sure that you work with gdb versions >= 10. We have found that earlier versions are only partially compatible with the VSCode debugging interface and crash when inspecting the data of Fortran derived types

---


## Compatibility

Parallel MODFLOW has been built successfully with the following configurations:

| Operating System      |   Toolchain |      MPI      |  PETSc |
|-----------------------|-------------|---------------|--------|
| MS Windows            | ?           | ?             | ?      |
| WSL2 (Ubuntu 20.04.5) | gcc 9.4.0   | OpenMPI 4.0.3 | 3.18.2 |
| Ubuntu 22.04          | gcc 9.5.0   | OpenMPI 4.1.4 | 3.18.5 |
| macOS 12.6.3          | gcc 9.5.0   | OpenMPI 4.1.4 | 3.18.5 |

## Known issues

tbd