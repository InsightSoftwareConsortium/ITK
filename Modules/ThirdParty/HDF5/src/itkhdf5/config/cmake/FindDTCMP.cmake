# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindDTCMP
--------

Find the native DTCMP includes and library

This module defines

::

  DTCMP_INCLUDE_DIR, where to find DTCMP.h, etc.
  DTCMP_LIBRARIES, the libraries required to use DTCMP.
  DTCMP_FOUND, If false, do not try to use DTCMP.

also defined, but not for general use are

::

  DTCMP_LIBRARY, where to find the DTCMP library.
#]=======================================================================]

if(DEFINED ENV{MFU_ROOT})
  set(ENV{MFU_INCLUDE} "$ENV{MFU_ROOT}/include")
  set(ENV{MFU_LIB} "$ENV{MFU_ROOT}/lib")
  set(ENV{MFU_LIB64} "$ENV{MFU_ROOT}/lib64")
else()
  message("DTCMP_LIBRARY: If you have problems building this library,\nconsider setting the MFU_ROOT environment variable to indicate\nwhere to find the support libraries and header files!")
endif()

find_path(DTCMP_INCLUDE_DIR
  NAMES dtcmp.h
  HINTS ENV MFU_INCLUDE)

find_library(DTCMP_LIBRARY
  NAMES dtcmp
  HINTS ENV MFU_LIB ENV MFU_LIB64)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(DTCMP REQUIRED_VARS DTCMP_LIBRARY DTCMP_INCLUDE_DIR)

if(DTCMP_FOUND)
  set(DTCMP_LIBRARIES ${DTCMP_LIBRARY} )
endif()

mark_as_advanced(DTCMP_INCLUDE_DIR DTCMP_LIBRARY)
