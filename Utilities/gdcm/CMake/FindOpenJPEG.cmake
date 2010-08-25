# - Try to find the OpenJPEG (JPEG 2000) library
#
# Read-Only variables:
#  OPENJPEG_FOUND - system has the OpenJPEG library
#  OPENJPEG_INCLUDE_DIR - the OpenJPEG include directory
#  OPENJPEG_LIBRARIES - The libraries needed to use OpenJPEG

#=============================================================================
# Copyright 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

# Try first to locate a cmake config file
FIND_PACKAGE(OpenJPEG QUIET NO_MODULE)

if( NOT OpenJPEG_DIR )
SET(OPENJPEG_MAJOR_VERSION 1) # FIXME ?
FIND_PATH(OPENJPEG_INCLUDE_DIR
  NAMES openjpeg.h #openjpeg-1.0/openjpeg.h
  PATHS /usr/local/include
  /usr/local/include/openjpeg-1.0
  /usr/include
  /usr/include/openjpeg-1.0
  )

FIND_LIBRARY(OPENJPEG_LIBRARY
  NAMES openjpeg
  )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(OpenJPEG DEFAULT_MSG
  OPENJPEG_LIBRARY
  OPENJPEG_INCLUDE_DIR
)

IF(OPENJPEG_FOUND)
  SET(OPENJPEG_LIBRARIES ${OPENJPEG_LIBRARY})
  SET(OPENJPEG_INCLUDE_DIRS ${OPENJPEG_INCLUDE_DIR})
ENDIF()

MARK_AS_ADVANCED(
  OPENJPEG_LIBRARY
  OPENJPEG_INCLUDE_DIR
  )
endif()
