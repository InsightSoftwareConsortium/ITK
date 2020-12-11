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
#########################################################################

# - Derived from the FindTiff.cmake and FindJPEG.cmake that is included with cmake
# FindSZIP

# Find the native SZIP includes and library

# Imported targets
##################

# This module defines the following :prop_tgt:`IMPORTED` targets:
#
# SZIP::SZIP
#  The SZIP library, if found.
#
# Result variables
###################

# This module will set the following variables in your project:

#  SZIP_FOUND, true if the SZIP headers and libraries were found.
#  SZIP_INCLUDE_DIR, the directory containing the SZIP headers.
#  SZIP_INCLUDE_DIRS, the directory containing the SZIP headers.
#  SZIP_LIBRARIES, libraries to link against to use SZIP.

# Cache variables
#################

# The following variables may also be set:

#  SZIP_LIBRARY, where to find the SZIP library.
#  SZIP_LIBRARY_DEBUG - Debug version of SZIP library
#  SZIP_LIBRARY_RELEASE - Release Version of SZIP library

# message (STATUS "Finding SZIP library and headers..." )
#########################################################################


find_path(SZIP_INCLUDE_DIR szlib.h)

set(szip_names ${SZIP_NAMES} sz szip szip-static libsz libszip libszip-static)
foreach(name ${szip_names})
  list(APPEND szip_names_debug "${name}d")
endforeach()

if(NOT SZIP_LIBRARY)
  find_library(SZIP_LIBRARY_RELEASE NAMES ${szip_names})
  find_library(SZIP_LIBRARY_DEBUG NAMES ${szip_names_debug})
  include(SelectLibraryConfigurations)
  select_library_configurations(SZIP)
  mark_as_advanced(SZIP_LIBRARY_RELEASE SZIP_LIBRARY_DEBUG)
endif()
unset(szip_names)
unset(szip_names_debug)

if(SZIP_INCLUDE_DIR AND EXISTS "${SZIP_INCLUDE_DIR}/SZconfig.h")
    file(STRINGS "${SZIP_INCLUDE_DIR}/SZconfig.h" szip_version_str
         REGEX "^#define[\t ]+SZIP_PACKAGE_VERSION[\t ]+.*")

    string(REGEX REPLACE "^#define[\t ]+SZIP_PACKAGE_VERSION[\t ]+([0-9]+).*"
           "\\1" SZIP_VERSION "${szip_version_str}")
    unset(szip_version_str)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SZIP
  REQUIRED_VARS SZIP_LIBRARY SZIP_INCLUDE_DIR
  VERSION_VAR SZIP_VERSION)

if(SZIP_FOUND)
  set(SZIP_LIBRARIES ${SZIP_LIBRARY})
  set(SZIP_INCLUDE_DIRS "${SZIP_INCLUDE_DIR}")

  if(NOT TARGET SZIP::SZIP)
    add_library(SZIP::SZIP UNKNOWN IMPORTED)
    if(SZIP_INCLUDE_DIRS)
      set_target_properties(SZIP::SZIP PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${SZIP_INCLUDE_DIRS}")
    endif()
    if(EXISTS "${SZIP_LIBRARY}")
      set_target_properties(SZIP::SZIP PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES "C"
        IMPORTED_LOCATION "${SZIP_LIBRARY}")
    endif()
    if(EXISTS "${SZIP_LIBRARY_RELEASE}")
      set_property(TARGET SZIP::SZIP APPEND PROPERTY
        IMPORTED_CONFIGURATIONS RELEASE)
      set_target_properties(SZIP::SZIP PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
        IMPORTED_LOCATION_RELEASE "${SZIP_LIBRARY_RELEASE}")
    endif()
    if(EXISTS "${SZIP_LIBRARY_DEBUG}")
      set_property(TARGET SZIP::SZIP APPEND PROPERTY
        IMPORTED_CONFIGURATIONS DEBUG)
      set_target_properties(SZIP::SZIP PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES_DEBUG "C"
        IMPORTED_LOCATION_DEBUG "${SZIP_LIBRARY_DEBUG}")
    endif()
  endif()
endif()

mark_as_advanced(SZIP_LIBRARY SZIP_INCLUDE_DIR)

# Report the results.
if (NOT SZIP_FOUND)
  set (SZIP_DIR_MESSAGE
      "SZip was not found. Make sure SZIP_LIBRARY and SZIP_INCLUDE_DIR are set or set the SZIP_INSTALL environment variable."
  )
  if (NOT SZIP_FIND_QUIETLY)
    message (STATUS "${SZIP_DIR_MESSAGE}")
  else ()
    if (SZIP_FIND_REQUIRED)
      message (FATAL_ERROR "SZip was NOT found and is Required by this project")
    endif ()
  endif ()
endif ()
