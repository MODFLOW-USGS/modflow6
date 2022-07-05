REM SPDX-FileCopyrightText: 2020 Intel Corporation
REM
REM SPDX-License-Identifier: MIT

set VS_VER=%1

@call "C:\Program Files (x86)\Intel\oneAPI\setvars-vcvarsall.bat" %VS_VER%

for /f "tokens=* usebackq" %%f in (`dir /b "C:\Program Files (x86)\Intel\oneAPI\compiler\" ^| findstr /V latest ^| sort`) do @set "LATEST_VERSION=%%f"
@call "C:\Program Files (x86)\Intel\oneAPI\compiler\%LATEST_VERSION%\env\vars.bat"

echo "Visual Studio Version: %VS_VER%"
echo "OneAPI version: %LATEST_VERSION%"
echo "C:\Program Files (x86)\Intel\oneAPI\compiler\%LATEST_VERSION%\env\vars.bat"
echo "OneAPI root directory: %ONEAPI_ROOT%"
where ifort.exe

meson setup builddir -Ddebug=false --prefix=%CD% --libdir=bin
meson install -C builddir
cd autotest
pytest -v --durations=0 get_exes.py
