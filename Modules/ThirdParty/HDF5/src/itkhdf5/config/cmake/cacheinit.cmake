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

set (HDF_PACKAGE_NAMESPACE "hdf5::" CACHE STRING "Name for HDF package namespace (can be empty)" FORCE)

set (HDF5_BUILD_CPP_LIB ON CACHE BOOL "Build C++ support" FORCE)

set (HDF5_BUILD_FORTRAN ON CACHE BOOL "Build FORTRAN support" FORCE)

set (HDF5_BUILD_JAVA ON CACHE BOOL "Build JAVA support" FORCE)

set (HDF5_INSTALL_MOD_FORTRAN "NO" CACHE STRING "Copy FORTRAN mod files to include directory (NO SHARED STATIC)" FORCE)
set_property (CACHE HDF5_INSTALL_MOD_FORTRAN PROPERTY STRINGS NO SHARED STATIC)

set (HDF5_BUILD_GENERATORS ON CACHE BOOL "Build Test Generators" FORCE)

set (MPIEXEC_MAX_NUMPROCS "4" CACHE STRING "Minimum number of processes for HDF parallel tests" FORCE)

set (HDF5_ENABLE_ALL_WARNINGS ON CACHE BOOL "Enable all warnings" FORCE)

set (HDF_TEST_EXPRESS "2" CACHE STRING "Control testing framework (0-3)" FORCE)

set (HDF5_MINGW_STATIC_GCC_LIBS ON CACHE BOOL "Statically link libgcc/libstdc++" FORCE)

set (HDF5_ALLOW_EXTERNAL_SUPPORT "TGZ" CACHE STRING "Allow External Library Building (NO GIT TGZ)" FORCE)
set_property (CACHE HDF5_ALLOW_EXTERNAL_SUPPORT PROPERTY STRINGS NO GIT TGZ)

########################
# compression options
########################

set (ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of ZLIB package" FORCE)
set (ZLIB_TGZ_NAME "ZLib.tar.gz" CACHE STRING "Use HDF5_ZLib from compressed file" FORCE)
set (ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.2.13" CACHE STRING "Use ZLIB from original location" FORCE)
set (ZLIB_TGZ_ORIGNAME "zlib-1.2.13.tar.gz" CACHE STRING "Use ZLIB from original compressed file" FORCE)
set (ZLIB_USE_LOCALCONTENT ON CACHE BOOL "Use local file for ZLIB FetchContent" FORCE)

set (LIBAEC_PACKAGE_NAME "libaec" CACHE STRING "Name of AEC SZIP package" FORCE)
set (SZAEC_TGZ_NAME "LIBAEC.tar.gz" CACHE STRING "Use SZip AEC from compressed file" FORCE)
set (LIBAEC_TGZ_ORIGPATH "https://github.com/MathisRosenhauer/libaec/releases/download/v1.0.6" CACHE STRING "Use LIBAEC from original location" FORCE)
set (LIBAEC_TGZ_ORIGNAME "libaec-1.0.6.tar.gz" CACHE STRING "Use LIBAEC from original compressed file" FORCE)
set (LIBAEC_USE_LOCALCONTENT ON CACHE BOOL "Use local file for LIBAEC FetchContent" FORCE)

########################
# API test options
########################
set (KWSYS_TGZ_ORIGPATH "https://gitlab.kitware.com/utils/kwsys/-/archive/master" CACHE STRING "Use KWSYS from original location" FORCE)
set (KWSYS_TGZ_ORIGNAME "kwsys-master.tar.gz" CACHE STRING "Use KWSYS from original compressed file" FORCE)
set (KWSYS_USE_LOCALCONTENT OFF CACHE BOOL "Use local file for KWSYS FetchContent" FORCE)

########################
# filter plugin options
########################

set (PLUGIN_TGZ_NAME "hdf5_plugins.tar.gz" CACHE STRING "Use PLUGINS from compressed file" FORCE)

set (PLUGIN_PACKAGE_NAME "pl" CACHE STRING "Name of PLUGIN package" FORCE)

#############
# bitshuffle
#############

set (BSHUF_GIT_URL "https://someurl/bitshuffle.git" CACHE STRING "Use BSHUF from HDF Group repository" FORCE)
set (BSHUF_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BSHUF_TGZ_NAME "bitshuffle.tar.gz" CACHE STRING "Use BSHUF from compressed file" FORCE)

set (BSHUF_PACKAGE_NAME "bshuf" CACHE STRING "Name of BSHUF package" FORCE)

########
# blosc
########

set (BLOSC_GIT_URL "https://github.com/Blosc/c-blosc.git" CACHE STRING "Use BLOSC from GitHub repository" FORCE)
set (BLOSC_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BLOSC_TGZ_NAME "c-blosc.tar.gz" CACHE STRING "Use BLOSC from compressed file" FORCE)

set (BLOSC_PACKAGE_NAME "blosc" CACHE STRING "Name of BLOSC package" FORCE)

set (BLOSC_ZLIB_GIT_URL "https://someurl/zlib.git" CACHE STRING "Use BLOSC_ZLIB from HDF Group repository" FORCE)
set (BLOSC_ZLIB_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BLOSC_ZLIB_TGZ_NAME "ZLib.tar.gz" CACHE STRING "Use BLOSC_ZLib from compressed file" FORCE)

set (BLOSC_ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of BLOSC_ZLIB package" FORCE)

########
# bzip2
########

set (BZ2_GIT_URL "https://someurl/bzip2.git" CACHE STRING "Use BZ2 from HDF Group repository" FORCE)
set (BZ2_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (BZ2_TGZ_NAME "BZ2.tar.gz" CACHE STRING "Use BZ2 from compressed file" FORCE)

set (BZ2_PACKAGE_NAME "bz2" CACHE STRING "Name of BZ2 package" FORCE)

########
# fpzip
########

set (FPZIP_GIT_URL "https://https://github.com/LLNL/fpzip" CACHE STRING "Use FPZIP from GitHub repository" FORCE)
set (FPZIP_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (FPZIP_TGZ_NAME "fpzip.tar.gz" CACHE STRING "Use FPZIP from compressed file" FORCE)

set (FPZIP_PACKAGE_NAME "fpzip" CACHE STRING "Name of FPZIP package" FORCE)

#######
# jpeg
#######

set (JPEG_GIT_URL "https://someurl/jpeg.git" CACHE STRING "Use JPEG from HDF Group repository" FORCE)
set (JPEG_GIT_BRANCH "jpeg9c" CACHE STRING "" FORCE)

#set (JPEG_TGZ_NAME "JPEG9c.tar.gz" CACHE STRING "Use JPEG from compressed file" FORCE)
set (JPEG_TGZ_NAME "JPEG.tar.gz" CACHE STRING "Use JPEG from compressed file" FORCE)

set (JPEG_PACKAGE_NAME "jpeg" CACHE STRING "Name of JPEG package" FORCE)

######
# lz4
######

set (BUILD_LZ4_LIBRARY_SOURCE ON CACHE BOOL "build the lz4 library within the plugin" FORCE)

set (LZ4_GIT_URL "https://someurl/lz4.git" CACHE STRING "Use LZ4 from HDF Group repository" FORCE)
set (LZ4_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (LZ4_TGZ_NAME "lz4.tar.gz" CACHE STRING "Use LZ4 from compressed file" FORCE)

set (LZ4_PACKAGE_NAME "lz4" CACHE STRING "Name of LZ4 package" FORCE)

######
# lzf
######

set (LZF_GIT_URL "https://someurl/lzf.git" CACHE STRING "Use LZF from HDF Group repository" FORCE)
set (LZF_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (LZF_TGZ_NAME "lzf.tar.gz" CACHE STRING "Use LZF from compressed file" FORCE)

set (LZF_PACKAGE_NAME "lzf" CACHE STRING "Name of LZF package" FORCE)

#########
# mafisc
#########

#set (BUILD_MAFISC_LIBRARY_SOURCE OFF CACHE BOOL "build the mafisc library within the plugin" FORCE)

#set (MAFISC_PACKAGE_NAME "mafisc" CACHE STRING "Name of MAFISC package" FORCE)

#####
# sz
#####

set (SZ_GIT_URL "https://github.com/disheng222/SZ" CACHE STRING "Use SZ filter from GitHub repository" FORCE)
set (SZ_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (SZ_TGZ_NAME "szf.tar.gz" CACHE STRING "Use SZ filter from compressed file" FORCE)

set (SZ_PACKAGE_NAME "SZ" CACHE STRING "Name of SZ filter package" FORCE)

######
# zfp
######

set (ZFP_GIT_URL "https://github.com/LLNL/zfp.git" CACHE STRING "Use ZFP from GitHub repository" FORCE)
set (ZFP_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (ZFP_TGZ_NAME "zfp.tar.gz" CACHE STRING "Use ZFP from compressed file" FORCE)

set (ZFP_PACKAGE_NAME "zfp" CACHE STRING "Name of ZFP package" FORCE)

######
# zstd
######

set (ZSTD_GIT_URL "https://github.com/facebook/zstd" CACHE STRING "Use ZSTD from repository" FORCE)
set (ZSTD_GIT_BRANCH "dev" CACHE STRING "" FORCE)

set (ZSTD_TGZ_NAME "zstd.tar.gz" CACHE STRING "Use ZSTD from compressed file" FORCE)

set (ZSTD_PACKAGE_NAME "zstd" CACHE STRING "Name of ZSTD package" FORCE)

