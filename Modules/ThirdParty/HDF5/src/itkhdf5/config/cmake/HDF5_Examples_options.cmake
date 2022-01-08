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
#############################################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file        ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")                 ###
####  DEFAULT:                                                                            ###
####         BUILD_SHARED_LIBS:BOOL=OFF                                                   ###
####         HDF_BUILD_C:BOOL=ON                                                          ###
####         HDF_BUILD_CXX:BOOL=OFF                                                       ###
####         HDF_BUILD_FORTRAN:BOOL=OFF                                                   ###
####         HDF_BUILD_JAVA:BOOL=OFF                                                      ###
####         HDF_BUILD_FILTERS:BOOL=OFF                                                   ###
####         BUILD_TESTING:BOOL=OFF                                                       ###
####         HDF_ENABLE_PARALLEL:BOOL=OFF                                                 ###
####         HDF_ENABLE_THREADSAFE:BOOL=OFF                                               ###
#############################################################################################

### uncomment/comment and change the following lines for other configuration options
### build with shared libraries
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=ON")

#############################################################################################
####      languages       ####
### disable C builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_C:BOOL=OFF")

### enable C++ builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_CXX:BOOL=ON")

### enable Fortran builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_FORTRAN:BOOL=ON")

### enable JAVA builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_JAVA:BOOL=ON")

#############################################################################################
### enable FILTERS builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_FILTERS:BOOL=ON")
### default HDF5_PLUGIN_PATH to where the filter libraries are located
#set(ENV{HDF5_PLUGIN_PATH} "${INSTALLDIR}/lib/plugin")

#############################################################################################
### enable parallel program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_ENABLE_PARALLEL:BOOL=ON")


#############################################################################################
### enable threadsafe program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_ENABLE_THREADSAFE:BOOL=ON")


#############################################################################################
### enable test program builds, requires reference files in testfiles subdirectory
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=ON")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCOMPARE_TESTING:BOOL=ON")

#############################################################################################
