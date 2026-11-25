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
#########################################################################

# - Derived from the FindTiff.cmake and FindJPEG.cmake that is included with cmake
# FindMFU

# Find the native MFU includes and library

# Imported targets
##################

# This module defines the following :prop_tgt:`IMPORTED` targets:
#
# MFU::MFU
#  The MFU library, if found.
#
# Result variables
###################

# This module will set the following variables in your project:

#  MFU_FOUND, true if the MFU headers and libraries were found.
#  MFU_INCLUDE_DIR, the directory containing the MFU headers.
#  MFU_INCLUDE_DIRS, the directory containing the MFU headers.
#  MFU_LIBRARIES, libraries to link against to use MFU.

# Cache variables
#################

# The following variables may also be set:

#  MFU_LIBRARY, where to find the MFU library.
# message (STATUS "Finding MFU library and headers..." )
#########################################################################



FIND_PATH(MFU_INCLUDE_DIR
  NAMES mfu.h
  HINTS "$ENV{MFU_ROOT}/include"
)
FIND_LIBRARY(MFU_LIBRARY
  NAMES mfu
  HINTS "$ENV{MFU_ROOT}/lib64"
)

if(NOT MFU_LIBRARY)
  set(mfu_names ${MFU_NAMES} mfu libmfu)
  find_library(MFU_LIBRARY NAMES ${mfu_names})
  include(SelectLibraryConfigurations)
  select_library_configurations(MFU)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MFU
  REQUIRED_VARS MFU_LIBRARY MFU_INCLUDE_DIR)

if(MFU_FOUND)
  set(MFU_LIBRARIES "${MFU_LIBRARY}")
  set(MFU_INCLUDE_DIRS "${MFU_INCLUDE_DIR}")
  set(LL_PATH "$ENV{MFU_ROOT}/lib64:$ENV{MFU_ROOT}/lib")
  if(NOT TARGET MFU::MFU)
    add_library(MFU::MFU UNKNOWN IMPORTED)
    if(MFU_INCLUDE_DIRS)
      set_target_properties(MFU::MFU PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${MFU_INCLUDE_DIRS}")
    endif()
    if(EXISTS "${MFU_LIBRARY}")
      set_target_properties(MFU::MFU PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES "C"
        IMPORTED_LOCATION "${MFU_LIBRARY}")
    endif()
  endif()
endif()

# Report the results.
if (NOT MFU_FOUND)
  set (MFU_DIR_MESSAGE
      "Mfu was not found. Make sure MFU_LIBRARY and MFU_INCLUDE_DIR are set or set the MFU_INSTALL environment variable."
  )
  if (NOT MFU_FIND_QUIETLY)
    message (VERBOSE "${MFU_DIR_MESSAGE}")
  else ()
    if (MFU_FIND_REQUIRED)
      message (FATAL_ERROR "MFU was NOT found and is required.")
    endif ()
  endif ()
endif ()
