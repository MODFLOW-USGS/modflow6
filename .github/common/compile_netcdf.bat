cd "%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build"

:: BUILD/INSTALL STATIC LIBS
cmake --fresh -G Ninja -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake --build .
cmake –install .

:: BUILD/INSTALL SHARED LIBS
cmake --fresh -G Ninja -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=1 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake --build .
cmake –install .
