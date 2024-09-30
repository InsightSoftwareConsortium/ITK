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
########################################################
#  Include file for user options
########################################################

# To use this option, copy both the macro and option code
# into the root UserMacros.cmake file. Then enable the option,
# using the command line add "-DBUILD_STATIC_CRT_LIBS:BOOL=ON"
# OR add an include to the root UserMacros.cmake file:
# INCLUDE(path_to_file/WINDOWS_MT.cmake)

#-----------------------------------------------------------------------------
# Option to Build with Static CRT libraries on Windows (USE WITH CAUTION!!!)
#-----------------------------------------------------------------------------
option (HDF5_BUILD_STATIC_CRT_LIBS "Build With Static Windows CRT Libraries" OFF)
mark_as_advanced (HDF5_BUILD_STATIC_CRT_LIBS)
if (HDF5_BUILD_STATIC_CRT_LIBS)
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif ()
