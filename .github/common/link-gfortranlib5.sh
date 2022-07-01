#! /bin/bash

FCDIR=/c/ProgramData/Chocolatey/bin
LNDIR=/c/ProgramData/Chocolatey/lib/mingw/tools/install/mingw64/bin
if [ -d "$FCDIR" ] && [ -f "$LNDIR/libgfortran-5.dll" ] && [ ! -f "$FCDIR/libgfortran-5.dll" ]; then
    ln -s "$LNDIR/libgfortran-5.dll" "$FCDIR/libgfortran-5.dll"
fi
