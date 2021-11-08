cmake_minimum_required(VERSION 3.12)

project(modflow Fortran)

include(GNUInstallDirs)

# check for HDF5
# set HDF5_ROOT or CMAKE_PREFIX_PATH using environment or cache variable
# ie
#   set CMAKE_PREFIX_PATH=C:/Program Files/HDF_Group/HDF5/1.12.1/share/cmake/hdf5
#   cmake ..
# or
#   cmake -D "CMAKE_PREFIX_PATH:PATH=C:/Program Files/HDF_Group/HDF5/1.12.1/share/cmake/hdf5" ..
find_package(HDF5 NAMES hdf5 COMPONENTS Fortran static)

add_subdirectory(src)
