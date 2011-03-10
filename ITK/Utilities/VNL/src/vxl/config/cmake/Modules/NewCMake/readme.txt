This directory contains Module files which will be available from the CMake distribution in the next release.
Any files in this directory should be identical to the latest versions in the CMake repository.

To facilitate the deprecation of modules, place code similar to the
following in the FindXXX.cmake as soon as it becomes part of a CMake
release:

# FIXME: When cmake_minimum_version reaches 2.6.2 the FindXXX
#        module in this directory is not needed anymore.
IF(CMAKE_MINIMUM_REQUIRED_VERSION GREATER 2.6.1)
  MESSAGE(FATAL_ERROR
    "FindXXX not needed in vxl; it is now available in CMake.")
ENDIF(CMAKE_MINIMUM_REQUIRED_VERSION GREATER 2.6.1)
