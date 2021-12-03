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

########################
# EXTERNAL cache entries
########################

set (CMAKE_INSTALL_FRAMEWORK_PREFIX "Library/Frameworks" CACHE STRING "Frameworks installation directory" FORCE)

set (HDF_PACKAGE_EXT "" CACHE STRING "Name of HDF package extension" FORCE)

set (HDF_PACKAGE_NAMESPACE "hdf5::" CACHE STRING "Name for HDF package namespace (can be empty)" FORCE)

set (HDF5_BUILD_CPP_LIB ON CACHE BOOL "Build C++ support" FORCE)

set (HDF5_BUILD_FORTRAN ON CACHE BOOL "Build FORTRAN support" FORCE)

set (HDF5_INSTALL_MOD_FORTRAN "NO" CACHE STRING "Copy FORTRAN mod files to include directory (NO SHARED STATIC)" FORCE)
set_property (CACHE HDF5_INSTALL_MOD_FORTRAN PROPERTY STRINGS NO SHARED STATIC)

set (HDF5_BUILD_GENERATORS ON CACHE BOOL "Build Test Generators" FORCE)

set (HDF5_ENABLE_Z_LIB_SUPPORT ON CACHE BOOL "Enable Zlib Filters" FORCE)

set (HDF5_ENABLE_SZIP_SUPPORT ON CACHE BOOL "Use SZip Filter" FORCE)

set (HDF5_ENABLE_SZIP_ENCODING ON CACHE BOOL "Use SZip Encoding" FORCE)

set (MPIEXEC_MAX_NUMPROCS "4" CACHE STRING "Minimum number of processes for HDF parallel tests" FORCE)

set (HDF5_ENABLE_ALL_WARNINGS ON CACHE BOOL "Enable all warnings" FORCE)

set (HDF_TEST_EXPRESS "2" CACHE STRING "Control testing framework (0-3)" FORCE)

set (HDF5_MINGW_STATIC_GCC_LIBS ON CACHE BOOL "Statically link libgcc/libstdc++" FORCE)

set (HDF5_ALLOW_EXTERNAL_SUPPORT "NO" CACHE STRING "Allow External Library Building (NO GIT TGZ)" FORCE)
set_property (CACHE HDF5_ALLOW_EXTERNAL_SUPPORT PROPERTY STRINGS NO GIT TGZ)

set (ZLIB_TGZ_NAME "ZLib.tar.gz" CACHE STRING "Use ZLib from compressed file" FORCE)
set (SZIP_TGZ_NAME "SZip.tar.gz" CACHE STRING "Use SZip from compressed file" FORCE)
set (SZAEC_TGZ_NAME "LIBAEC.tar.gz" CACHE STRING "Use SZip AEC from compressed file" FORCE)
set (USE_LIBAEC ON CACHE BOOL "Use libaec szip replacement" FORCE)

set (ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of ZLIB package" FORCE)
set (LIBAEC_PACKAGE_NAME "libaec" CACHE STRING "Name of AEC SZIP package" FORCE)
set (SZIP_PACKAGE_NAME "szip" CACHE STRING "Name of SZIP package" FORCE)

#######################
# filter plugin options
#######################
set (PLUGIN_TGZ_NAME "hdf5_plugins.tar.gz" CACHE STRING "Use PLUGINS from compressed file" FORCE)

set (PLUGIN_PACKAGE_NAME "pl" CACHE STRING "Name of PLUGIN package" FORCE)

############
# bitshuffle
###########

set (BSHUF_GIT_URL "https://git@bitbucket.hdfgroup.org/scm/test/bitshuffle.git" CACHE STRING "Use BSHUF from HDF repository" FORCE)
set (BSHUF_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BSHUF_TGZ_NAME "bitshuffle.tar.gz" CACHE STRING "Use BSHUF from compressed file" FORCE)

set (BSHUF_PACKAGE_NAME "bshuf" CACHE STRING "Name of BSHUF package" FORCE)

#######
# blosc
#######

set (BLOSC_GIT_URL "https://github.com/Blosc/c-blosc.git" CACHE STRING "Use BLOSC from Github" FORCE)
set (BLOSC_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BLOSC_TGZ_NAME "c-blosc.tar.gz" CACHE STRING "Use BLOSC from compressed file" FORCE)

set (BLOSC_PACKAGE_NAME "blosc" CACHE STRING "Name of BLOSC package" FORCE)

set (ZLIB_GIT_URL "https://git@bitbucket.hdfgroup.org/scm/test/zlib.git" CACHE STRING "Use ZLIB from HDF repo" FORCE)
set (ZLIB_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (ZLIB_TGZ_NAME "ZLib.tar.gz" CACHE STRING "Use ZLib from compressed file" FORCE)

set (ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of ZLIB package" FORCE)

#######
# bzip2
######
#
set (BZ2_GIT_URL "https://git@bitbucket.hdfgroup.org/scm/test/bzip2.git" CACHE STRING "Use BZ2 from HDF repository" FORCE)
set (BZ2_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BZ2_TGZ_NAME "BZ2.tar.gz" CACHE STRING "Use BZ2 from compressed file" FORCE)

set (BZ2_PACKAGE_NAME "bz2" CACHE STRING "Name of BZ2 package" FORCE)

#######
# fpzip
#######

set (FPZIP_GIT_URL "https://https://github.com/LLNL/fpzip" CACHE STRING "Use FPZIP from github repository" FORCE)
set (FPZIP_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (FPZIP_TGZ_NAME "fpzip.tar.gz" CACHE STRING "Use FPZIP from compressed file" FORCE)

set (FPZIP_PACKAGE_NAME "fpzip" CACHE STRING "Name of FPZIP package" FORCE)

######
# jpeg
######

set (JPEG_GIT_URL "https://git@bitbucket.hdfgroup.org/scm/test/jpeg.git" CACHE STRING "Use JPEG from HDF repository" FORCE)
set (JPEG_GIT_BRANCH "jpeg9c" CACHE STRING "" FORCE)

#set (JPEG_TGZ_NAME "JPEG9c.tar.gz" CACHE STRING "Use JPEG from compressed file" FORCE)
set (JPEG_TGZ_NAME "JPEG.tar.gz" CACHE STRING "Use JPEG from compressed file" FORCE)

set (JPEG_PACKAGE_NAME "jpeg" CACHE STRING "Name of JPEG package" FORCE)

######
# lz4
######

set (BUILD_LZ4_LIBRARY_SOURCE ON CACHE BOOL "build the lz4 library within the plugin" FORCE)

set (LZ4_GIT_URL "https://git@bitbucket.hdfgroup.org/scm/test/lz4.git" CACHE STRING "Use LZ4 from HDF repository" FORCE)
set (LZ4_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (LZ4_TGZ_NAME "lz4.tar.gz" CACHE STRING "Use LZ4 from compressed file" FORCE)

set (LZ4_PACKAGE_NAME "lz4" CACHE STRING "Name of LZ4 package" FORCE)

######
# lzf
######

set (LZF_GIT_URL "https://git@bitbucket.hdfgroup.org/scm/test/lzf.git" CACHE STRING "Use LZF from HDF repository" FORCE)
set (LZF_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (LZF_TGZ_NAME "lzf.tar.gz" CACHE STRING "Use LZF from compressed file" FORCE)

set (LZF_PACKAGE_NAME "lzf" CACHE STRING "Name of LZF package" FORCE)

########
# mafisc
########

#set (BUILD_MAFISC_LIBRARY_SOURCE OFF CACHE BOOL "build the mafisc library within the plugin" FORCE)

#set (MAFISC_PACKAGE_NAME "mafisc" CACHE STRING "Name of MAFISC package" FORCE)

######
# szf
######

set (SZF_GIT_URL "https://github.com/disheng222/SZ" CACHE STRING "Use SZ from github repository" FORCE)
set (SZF_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (SZF_TGZ_NAME "szf.tar.gz" CACHE STRING "Use SZ from compressed file" FORCE)

set (SZF_PACKAGE_NAME "szf" CACHE STRING "Name of SZ package" FORCE)

######
# zfp
######

set (ZFP_GIT_URL "https://github.com/LLNL/zfp.git" CACHE STRING "Use ZFP from Github" FORCE)
set (ZFP_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (ZFP_TGZ_NAME "zfp.tar.gz" CACHE STRING "Use ZFP from compressed file" FORCE)

set (ZFP_PACKAGE_NAME "zfp" CACHE STRING "Name of ZFP package" FORCE)

