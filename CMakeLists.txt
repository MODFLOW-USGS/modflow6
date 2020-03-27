# CMake project file for mf6

##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(MF6 Fortran)

# Set the mf6 version
SET(VERSION 6.0.5)

# Add our local modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Uncomment if it is required that Fortran 90 is supported
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should review this file and make sure the flags
# are to your liking.
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

MESSAGE( ${FCNAME} )
MESSAGE( ${CMAKE_SOURCE_DIR} )
MESSAGE( ${CMAKE_MODULE_PATH} )
MESSAGE( ${CMAKE_Fortran_FLAGS} )


############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
SET(MF6EXE mf6)

# Define the library name
SET(MF6LIB mf6lib)

# define the
SET(BUILD_DIR ${CMAKE_SOURCE_DIR}/build)
file(MAKE_DIRECTORY ${BUILD_DIR})
MESSAGE( ${BUILD_DIR} )


# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${BUILD_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

# Have the .mod files placed in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

## mf6 library
#file(GLOB SRC_TOP
#       ${SRC}/*.f90
#       ${SRC}/*.fpp)
#file(GLOB_RECURSE SRC_NESTED
#       ${SRC}/Utilties/*.f90
#       ${SRC}/Utilities/*.fpp
#       ${SRC}/Timing/*.f90
#       ${SRC}/Timing/*.fpp
#       ${SRC}/Solution/*.f90
#       ${SRC}/Solution/*.fpp
#       ${SRC}/Exchange/*.f90
#       ${SRC}/Exchange/*.fpp
#       ${SRC}/Model/*.f90
#       ${SRC}/Model/*.fpp)
#SET(SRC_LIB ${SRC_NESTED} ${SRC_TOP})
#MESSAGE( ${SRC_LIB} )
#LIST(REMOVE_ITEM SRC_LIB ${SRC}/mf6.f90)
#LIST(REMOVE_ITEM SRC_LIB ${SRC}/mf6_duplicate.f90)
#MESSAGE( ${SRC_LIB} )

FILE(STRINGS ${CMAKE_SOURCE_DIR}/cmake_mf6_files.txt SRC_LIB)
MESSAGE( ${SRC_LIB} )
LIST(REMOVE_ITEM ${SRC_LIB} "src/mf6.f90")
MESSAGE( ${SRC_LIB} )

# This links mf6 to the mf6lib library
ADD_LIBRARY(${MF6LIB} STATIC ${SRC_LIB})

SET(SRC_MF6 ${SRC}/mf6.f90)
ADD_EXECUTABLE(${MF6EXE} ${SRC_MF6})

TARGET_LINK_LIBRARIES(${MF6EXE} ${MF6LIB})

INSTALL(TARGETS ${MF6EXE} RUNTIME DESTINATION ${BIN})