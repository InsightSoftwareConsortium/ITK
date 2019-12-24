#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
#############################################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file        ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")                 ###
#############################################################################################

### uncomment/comment and change the following lines for other configuration options

#############################################################################################
####      maximum parallel processor count for build and test       ####
#set (MAX_PROC_COUNT 8)

#############################################################################################
####      alternate toolsets       ####
#set (CMAKE_GENERATOR_TOOLSET "Intel C++ Compiler 17.0")

#############################################################################################
####      Only build static libraries       ####
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=OFF")
####      Only build shared libraries       ####
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DONLY_SHARED_LIBS:BOOL=OFF")
####      Add PIC option on linux/mac       ####
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_ANSI_CFLAGS:STRING=-fPIC")

#############################################################################################
####      fortran enabled      ####
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=ON")
####      fortran disabled      ####
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=OFF")

#############################################################################################
####      java enabled      ####
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_JAVA:BOOL=ON")
####      java disabled      ####
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_JAVA:BOOL=OFF")

#############################################################################################
### change install prefix (default use INSTALLDIR value)
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_INSTALL_PREFIX:PATH=${INSTALLDIR}")

#############################################################################################
####      ext libraries       ####

### ext libs from tgz
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=TGZ -DTGZPATH:PATH=${CTEST_SCRIPT_DIRECTORY}")
### ext libs from git
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=GIT")
### ext libs on system
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=NO")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DZLIB_LIBRARY:FILEPATH=some_location/lib/zlib.lib -DZLIB_INCLUDE_DIR:PATH=some_location/include")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DSZIP_LIBRARY:FILEPATH=some_location/lib/szlib.lib -DSZIP_INCLUDE_DIR:PATH=some_location/include")

### disable using ext zlib
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=OFF")
### disable using ext szip
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_SZIP_ENCODING:BOOL=OFF")

####      package examples       ####
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_PACK_EXAMPLES:BOOL=ON -DHDF5_EXAMPLES_COMPRESSED:STRING=HDF5Examples-1.10.13-Source.tar.gz -DHDF5_EXAMPLES_COMPRESSED_DIR:PATH=${CTEST_SCRIPT_DIRECTORY}")

#############################################################################################
### enable parallel builds
if (DEFINED MPI)
  set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_PARALLEL:BOOL=ON")
  set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_CPP_LIB:BOOL=OFF")
  set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_JAVA:BOOL=OFF")
  set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_THREADSAFE:BOOL=OFF")
endif()
#############################################################################################
### enable thread-safety builds

#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_THREADSAFE:BOOL=ON")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_PARALLEL:BOOL=OFF")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_CPP_LIB:BOOL=OFF")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=OFF")
#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_HL_LIB:BOOL=OFF")

#############################################################################################
### disable test program builds

#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=OFF")

#############################################################################################
### disable packaging

#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_NO_PACKAGES:BOOL=ON")
### Create install package with external libraries (szip, zlib)
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_PACKAGE_EXTLIBS:BOOL=ON")

#############################################################################################
### use a toolchain file

#set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_TOOLCHAIN_FILE:STRING=config/toolchain/intel.cmake")

#############################################################################################
