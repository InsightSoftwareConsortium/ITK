# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindCIRCLE
--------

Find the native CIRCLE includes and library

This module defines

::

  CIRCLE_INCLUDE_DIR, where to find CIRCLE.h, etc.
  CIRCLE_LIBRARIES, the libraries required to use CIRCLE.
  CIRCLE_FOUND, If false, do not try to use CIRCLE.

also defined, but not for general use are

::

  CIRCLE_LIBRARY, where to find the CIRCLE library.
#]=======================================================================]

if(DEFINED ENV{MFU_ROOT})
  set(ENV{MFU_INCLUDE} "$ENV{MFU_ROOT}/include")
  set(ENV{MFU_LIB} "$ENV{MFU_ROOT}/lib")
  set(ENV{MFU_LIB64} "$ENV{MFU_ROOT}/lib64")
else()
  message("CIRCLE_LIBRARY: If you have problems building this library,\nconsider setting the MFU_ROOT environment variable to indicate\nwhere to find the support libraries and header files!")
endif()

find_path(CIRCLE_INCLUDE_DIR
  NAMES libcircle.h
  HINTS ENV MFU_INCLUDE)

find_library(CIRCLE_LIBRARY
  NAMES circle
  HINTS ENV MFU_LIB ENV MFU_LIB64
  )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CIRCLE REQUIRED_VARS CIRCLE_LIBRARY CIRCLE_INCLUDE_DIR)

if(CIRCLE_FOUND)
  set(CIRCLE_LIBRARIES ${CIRCLE_LIBRARY} )
endif()

mark_as_advanced(CIRCLE_INCLUDE_DIR CIRCLE_LIBRARY)
