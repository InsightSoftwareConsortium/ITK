# Copyright 2011 by Kitware, Inc. All Rights Reserved. Please refer to
# KITWARE_LICENSE.TXT for licensing information, or contact General Counsel,
# Kitware, Inc., 28 Corporate Drive, Clifton Park, NY 12065.
#
# Author: Chuck Atkins <chuck dot atkins at kitware dot com>
#
# Locate the system installed OpenJPEG v2
#
# The following variables will be set:
#
# OPENJPEG2_FOUND       - Set to true if OpenJPEG v2 can be found
# OPENJPEG2_INCLUDE_DIR - The path to the OpenJPEG v2 header files
# OPENJPEG2_LIBRARIES   - The full path to the OpenJPEG v2 library
# OPENJPEG2_DEFINITIONS - You should ADD_DEFINITONS(${OPENJPEG2_DEFINITIONS})
#                         before compiling code that includes OpenJPEG2 library files.

if( NOT VXL_FORCE_V3P_OPENJPEG2 )
  if( NOT OPENJPEG2_FOUND )
    include(CheckTypeSize)
    include(CheckFunctionExists)

    find_path( OPENJPEG2_INCLUDE_DIR openjpeg.h )
#    message(STATUS "Looking for openjpeg.h - ${OPENJPEG2_INCLUDE_DIR}")

    if( OPENJPEG2_INCLUDE_DIR )
      #The opj_cio struct is only preset in the old v1 API
      set( CMAKE_REQUIRED_INCLUDES "${OPENJPEG2_INCLUDE_DIR}" )
      set( CMAKE_EXTRA_INCLUDE_FILES "openjpeg.h" )
      CHECK_TYPE_SIZE("struct opj_cio" STRUCT_OPJ_CIO)
      unset( CMAKE_REQUIRED_INCLUDES )
      unset( CMAKE_EXTRA_INCLUDE_FILES )
      if(HAVE_STRUCT_OPJ_CIO)
        set(OPENJPEG2_INCLUDE_V2 FALSE)
      else()
        set(OPENJPEG2_INCLUDE_V2 TRUE)
      endif()
#      message(STATUS "Checking OpenJPEG header for v2 API - " ${OPENJPEG2_INCLUDE_V2})
    endif()

    find_library( OPENJPEG2_LIBRARIES "libopenjpeg" )
#    message(STATUS "Looking for libopenjpeg - ${OPENJPEG2_LIBRARIES}")

    if( OPENJPEG2_LIBRARIES )
      #opj_cio_open is part of the old API and has been removed in v2
      set(CMAKE_REQUIRED_LIBRARIES "${OPENJPEG2_LIBRARIES}")
      CHECK_FUNCTION_EXISTS("opj_cio_open" HAVE_OPJ_CIO_OPEN)
      unset(CMAKE_REQUIRED_LIBRARIES)
      if(HAVE_OPJ_CIO_OPEN)
        set(OPENJPEG2_LIBRARIES_V2 FALSE)
      else()
        set(OPENJPEG2_LIBRARIES_V2 TRUE)
      endif()
#      message(STATUS "Checking OpenJPEG library for v2 API - " ${OPENJPEG2_LIBRARIES_V2})
    endif()
    include( FindPackageHandleStandardArgs )
    FIND_PACKAGE_HANDLE_STANDARD_ARGS( OPENJPEG2 OPENJPEG2_INCLUDE_DIR OPENJPEG2_INCLUDE_V2 OPENJPEG2_LIBRARIES OPENJPEG2_LIBRARIES_V2 )
  endif()
endif()

set(OPENJPEG2_DEFINITIONS "")


if(OPENJPEG2_FOUND)
  set(VXL_USING_NATIVE_OPENJPEG2 "YES")
else()
  if( EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/openjpeg2/openjpeg.h)
    set(OPENJPEG2_FOUND TRUE)
    set(OPENJPEG2_INCLUDE_DIR ${VXL_ROOT_SOURCE_DIR}/v3p/openjpeg2)
    set(OPENJPEG2_LIBRARIES openjpeg2)
    if (NOT BUILD_SHARED_LIBRARIES)
      set(OPENJPEG2_DEFINITIONS ${OPENJPEG2_DEFINITIONS} -DOPJ_STATIC)
    endif ()
  endif()
endif()

