#!/bin/bash

#SBATCH --job-name=denali-build
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --account=impd
#SBATCH --time=00:10:00
#SBATCH --output=slurm-%j.out
#SBATCH --chdir=../

set -euxo pipefail

# load appropriate modules
module switch PrgEnv-${PE_ENV,,} PrgEnv-intel
module load cray-petsc meson ninja
export PKG_CONFIG_PATH=/opt/cray/pe/mpt/7.7.19/gni/mpich-intel/16.0/lib/pkgconfig:/opt/cray/pe/petsc/3.14.5.0/real/INTEL/19.1/x86_skylake/lib/pkgconfig:$PKG_CONFIG_PATH

# list loaded modules
module list

# define the project root (expected to be cwd)
MODFLOW6ROOT=$(pwd)

# define the version
VERSION=$(cat "$MODFLOW6ROOT/version.txt") 
echo "MODFLOW 6 version: $VERSION"

# define paths relative to the root directory
BUILDDIR=$MODFLOW6ROOT/$PE_ENV-$VERSION
BINDIR=$BUILDDIR/src
TESTDIR=$MODFLOW6ROOT/.mf6minsim

# define the installation location
PREFIX=/home/software/denali/contrib/impd/apps/modflow/$VERSION/$PE_ENV/19.1.0.166

# build MODFLOW 6
CC=cc CXX=CC F77=ftn F90=ftn FC=ftn meson setup $BUILDDIR --prefix=$PREFIX --bindir=bin --libdir=lib -Dcray=true -Ddebug=false
meson compile -C $BUILDDIR

# install MODFLOW 6
meson install -C $BUILDDIR

# test MODFLOW 6 build
cd $TESTDIR

# serial run
$BINDIR/mf6

# parallel run
srun $BINDIR/mf6 -p