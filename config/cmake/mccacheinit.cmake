#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# This is the CMakeCache file.

#########################
# EXTERNAL cache entries
#########################

set (CMAKE_INSTALL_FRAMEWORK_PREFIX "Library/Frameworks" CACHE STRING "Frameworks installation directory" FORCE)

set (HDF_PACKAGE_EXT "" CACHE STRING "Name of HDF package extension" FORCE)

set (HDF_PACKAGE_NAMESPACE "hdf5::" CACHE STRING "Name for HDF package namespace" FORCE)

set (HDF5_BUILD_CPP_LIB ON CACHE BOOL "Build HDF5 C++ Library" FORCE)

set (HDF5_BUILD_FORTRAN ON CACHE BOOL "Build FORTRAN support" FORCE)

set (HDF5_BUILD_HL_LIB ON CACHE BOOL "Build HIGH Level HDF5 Library" FORCE)

set (HDF5_BUILD_TOOLS ON CACHE BOOL "Build HDF5 Tools" FORCE)

set (HDF5_BUILD_EXAMPLES ON CACHE BOOL "Build HDF5 Library Examples" FORCE)

set (HDF5_ENABLE_Z_LIB_SUPPORT ON CACHE BOOL "Enable Zlib Filters" FORCE)

set (HDF5_ENABLE_SZIP_SUPPORT ON CACHE BOOL "Use SZip Filter" FORCE)

set (HDF5_ENABLE_SZIP_ENCODING ON CACHE BOOL "Use SZip Encoding" FORCE)

set (MPIEXEC_MAX_NUMPROCS "4" CACHE STRING "Minimum number of processes for HDF parallel tests" FORCE)

set (HDF5_ENABLE_USING_MEMCHECKER ON CACHE BOOL "Indicate that a memory checker is used" FORCE)

set (HDF5_NO_PACKAGES ON CACHE BOOL "CPACK - Disable packaging" FORCE)

set (HDF_TEST_EXPRESS "2" CACHE STRING "Control testing framework (0-3)" FORCE)

set (HDF5_MINGW_STATIC_GCC_LIBS ON CACHE BOOL "Statically link libgcc/libstdc++" FORCE)

set (HDF5_ALLOW_EXTERNAL_SUPPORT "TGZ" CACHE STRING "Allow External Library Building (NO GIT TGZ)" FORCE)
set_property (CACHE HDF5_ALLOW_EXTERNAL_SUPPORT PROPERTY STRINGS NO GIT TGZ)

set (ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of ZLIB package" FORCE)
set (ZLIB_TGZ_NAME "ZLib.tar.gz" CACHE STRING "Use ZLib from compressed file" FORCE)

set (LIBAEC_PACKAGE_NAME "libaec" CACHE STRING "Name of AEC SZIP package" FORCE)
set (SZAEC_TGZ_NAME "LIBAEC.tar.gz" CACHE STRING "Use SZip AEC from compressed file" FORCE)

set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build Debug" FORCE)

set (CTEST_CONFIGURATION_TYPE "Debug" CACHE STRING "Build Debug" FORCE)
