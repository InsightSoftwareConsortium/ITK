#
# Root vxl
#
#   geoff.cross@crd.ge.com

CMAKE_MINIMUM_REQUIRED(VERSION 2.4.5)

# Support for CMake 2.6
IF( COMMAND CMAKE_POLICY )
  CMAKE_POLICY(SET CMP0003 NEW)
ENDIF( COMMAND CMAKE_POLICY )

PROJECT(vxl)

SET( LIBRARY_OUTPUT_PATH ${vxl_BINARY_DIR}/lib CACHE PATH
     "Output directory for the vxl libraries" )

IF( NOT EXECUTABLE_OUTPUT_PATH )
    SET( EXECUTABLE_OUTPUT_PATH "." )
ENDIF( NOT EXECUTABLE_OUTPUT_PATH )


# CMake support directory.
SET(VXL_CMAKE_DIR ${vxl_SOURCE_DIR}/config/cmake/Modules)

INCLUDE( ${VXL_CMAKE_DIR}/VXLStandardOptions.cmake )
INCLUDE( ${vxl_SOURCE_DIR}/config/cmake/config/vxl_utils.cmake )
# INCLUDE( ${vxl_SOURCE_DIR}/UseSTLPort.cmake )
INCLUDE(${vxl_SOURCE_DIR}/config/cmake/doxygen/doxygen.cmake)

# Copy the UseVXL.cmake file to the binary directory so that client
# projects don't need to find the source directory first. They can run
# the UseVXL.cmake from the vxl binary directory, and determine the
# vxl source directory by loading the cache.
CONFIGURE_FILE( ${VXL_CMAKE_DIR}/UseVXL.cmake
                ${vxl_BINARY_DIR}/UseVXL.cmake COPYONLY )

# Copy CTestCustom.cmake to top of build tree
CONFIGURE_FILE( ${VXL_CMAKE_DIR}/CTestCustom.cmake
  ${vxl_BINARY_DIR}/CTestCustom.cmake COPYONLY )

# Location of VXL's FindXXX.cmake CMake modules.
# This is identical to VXL_CMAKE_DIR.  Perhaps we should eliminate MODULE_PATH?
SET( MODULE_PATH ${vxl_SOURCE_DIR}/config/cmake/Modules CACHE STATIC "VXL module path" )

# For use in client projects that call UseVXL.cmake
SET (VXL_LIBRARY_PATH ${LIBRARY_OUTPUT_PATH} CACHE STATIC "Where all the vxl libraries are, for clients to use." )

# Options to add extra compiler and linker flags
#
# These options allow you to specify additional flags without
# affecting the default flags for a particular platform or build type.
# This is especially useful for adding extra warning flags.
SET( VXL_EXTRA_CMAKE_C_FLAGS             CACHE STRING "Extra flags appended to CMAKE_C_FLAGS" )
SET( VXL_EXTRA_CMAKE_CXX_FLAGS           CACHE STRING "Extra flags appended to CMAKE_CXX_FLAGS" )
SET( VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS    CACHE STRING "Extra flags appended to CMAKE_EXE_LINKER_FLAGS" )
SET( VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS CACHE STRING "Extra flags appended to CMAKE_MODULE_LINKER_FLAGS" )
SET( VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS CACHE STRING "Extra flags appended to CMAKE_SHARED_LINKER_FLAGS" )
SET( CMAKE_C_FLAGS             "${CMAKE_C_FLAGS} ${VXL_EXTRA_CMAKE_C_FLAGS}" )
SET( CMAKE_CXX_FLAGS           "${CMAKE_CXX_FLAGS} ${VXL_EXTRA_CMAKE_CXX_FLAGS}" )
SET( CMAKE_EXE_LINKER_FLAGS    "${CMAKE_EXE_LINKER_FLAGS} ${VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS}" )
SET( CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS}" )
SET( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS}" )


# Option to specify whether this is a build for the dashboard.  Each
# dashboard build should set BUILD_FOR_VXL_DASHBOARD to YES in the
# initial cache (set in the CTest script).
OPTION( BUILD_FOR_VXL_DASHBOARD "Is this a build for the dashboard?" NO )

# A variable to default some OPTIONs to YES for the dashboard, but to
# NO for other builds.
IF( BUILD_FOR_VXL_DASHBOARD )
  SET( YES_FOR_DASHBOARD "YES" )
ELSE( BUILD_FOR_VXL_DASHBOARD )
  SET( YES_FOR_DASHBOARD "NO" )
ENDIF( BUILD_FOR_VXL_DASHBOARD )


# Do platform-specific configuration.
IF(NOT VXL_NO_CMAKE_CONFIGURE)
  SUBDIRS(config/cmake/config)
ENDIF(NOT VXL_NO_CMAKE_CONFIGURE)


# Options to reduce build to just some core libraries
OPTION( BUILD_CORE_NUMERICS "Build VXL's numerics libraries" YES )
OPTION( BUILD_CORE_SERIALISATION "Build VXL's serialisation libraries" YES )
OPTION( BUILD_CORE_UTILITIES "Build VXL's utility libraries" YES )
OPTION( BUILD_CORE_GEOMETRY "Build VXL's geometry libraries" YES )
OPTION( BUILD_CORE_IMAGING "Build VXL's imaging libraries" YES )
IF(BUILD_CORE_NUMERICS)
  IF( BUILD_FOR_VXL_DASHBOARD )
    OPTION( BUILD_CORE_PROBABILITY "Build VXL's probability libraries (Experimental)" YES )
  ELSE( BUILD_FOR_VXL_DASHBOARD )
    OPTION( BUILD_CORE_PROBABILITY "Build VXL's probability libraries (Experimental)" NO )
  ENDIF( BUILD_FOR_VXL_DASHBOARD )
ENDIF(BUILD_CORE_NUMERICS)
SET (BUILD_CORE_ALL OFF CACHE INTERNAL "All Core libraries are being built")
IF (BUILD_CORE_GEOMETRY AND BUILD_CORE_NUMERICS)
  IF (BUILD_CORE_UTILITIES AND BUILD_CORE_SERIALISATION)
    IF (BUILD_CORE_IMAGING)
      SET (BUILD_CORE_ALL ON CACHE INTERNAL "All Core libraries are being built")
    ENDIF (BUILD_CORE_IMAGING)
  ENDIF (BUILD_CORE_UTILITIES AND BUILD_CORE_SERIALISATION)
ENDIF (BUILD_CORE_GEOMETRY AND BUILD_CORE_NUMERICS)

# Optionally use old error reporting methods, rather than exceptions.
# The main use is in vil which often uses/used null images to imply an error.
OPTION (VXL_LEGACY_ERROR_REPORTING "Use old error reporting methods rather than exceptions?" YES)
IF (VXL_LEGACY_ERROR_REPORTING)
  ADD_DEFINITIONS( -DVXL_LEGACY_ERROR_REPORTING )
ENDIF (VXL_LEGACY_ERROR_REPORTING)



# Options to build no longer maintained libraries.
# It is still useful to have the dashboard check that they are correct,
# but no-one is likely to work on improving test coverage or fixing memory leaks.
OPTION( BUILD_UNMAINTAINED_LIBRARIES "Build libraries that are no longer actively maintained?" ${YES_FOR_DASHBOARD} )

# Option to build Windows Unicode support, the string
# type of which is wchar_t, each character is a 16-bit unsigned integer. 
# 
IF(WIN32)
  IF(VXL_HAS_WIN_WCHAR_T)
    OPTION( VXL_USE_WIN_WCHAR_T "Build overloading functions that accept Windows wide char strings?" YES )
    IF(VXL_USE_WIN_WCHAR_T)   # Force it to be 0/1
      SET(VXL_USE_WIN_WCHAR_T 1)
    ELSE(VXL_USE_WIN_WCHAR_T)
      SET(VXL_USE_WIN_WCHAR_T 0)
    ENDIF(VXL_USE_WIN_WCHAR_T)
  ELSE(VXL_HAS_WIN_WCHAR_T)
    SET(VXL_USE_WIN_WCHAR_T 0)
  ENDIF(VXL_HAS_WIN_WCHAR_T)
ELSE(WIN32)
  # avoid empty macro definition
  SET(VXL_USE_WIN_WCHAR_T 0)
ENDIF(WIN32)

# Build the core vxl + support libraries
SUBDIRS(vcl v3p core)

# Optionally build the contributed libraries
IF( EXISTS ${vxl_SOURCE_DIR}/contrib/CMakeLists.txt )
  OPTION( BUILD_CONTRIB "Build the contributed libraries?" YES )
ENDIF( EXISTS ${vxl_SOURCE_DIR}/contrib/CMakeLists.txt )

IF( BUILD_CONTRIB AND BUILD_CORE_ALL)
  SUBDIRS( contrib )
ENDIF( BUILD_CONTRIB AND BUILD_CORE_ALL)

# Use the old configure script if the cache tells us to do so.
IF(VXL_NO_CMAKE_CONFIGURE)
  IF(RUN_CONFIGURE)
    EXEC_PROGRAM( ${vxl_SOURCE_DIR}/configure
      ${vxl_BINARY_DIR} )
  ENDIF(RUN_CONFIGURE)
ENDIF(VXL_NO_CMAKE_CONFIGURE)

# Some types of path names can cause havoc with regular expressions,
# so avoid those.
IF( ${vxl_BINARY_DIR} MATCHES  \\+ )
  MESSAGE(SEND_ERROR "You cannot have a + in your binary path")
ENDIF( ${vxl_BINARY_DIR} MATCHES  \\+ )
IF( ${vxl_SOURCE_DIR} MATCHES  \\+ )
  MESSAGE(SEND_ERROR "You cannot have a + in your source path")
ENDIF( ${vxl_SOURCE_DIR} MATCHES  \\+ )

# include CMakeListsLocal.txt from source directory if it exists
# also include it from the binary dir if different from source dir
IF( ${vxl_BINARY_DIR} MATCHES ${vxl_SOURCE_DIR} )
  INCLUDE( ${vxl_SOURCE_DIR}/CMakeListsLocal.txt OPTIONAL )
ELSE( ${vxl_BINARY_DIR} MATCHES ${vxl_SOURCE_DIR} )
  INCLUDE( ${vxl_SOURCE_DIR}/CMakeListsLocal.txt OPTIONAL )
  INCLUDE( ${vxl_BINARY_DIR}/CMakeListsLocal.txt OPTIONAL )
ENDIF( ${vxl_BINARY_DIR} MATCHES ${vxl_SOURCE_DIR} )

# Standard include directories.
SET(VXLCORE_INCLUDE_DIR ${vxl_BINARY_DIR}/core ${vxl_SOURCE_DIR}/core)
SET(VXLCORE_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/core)
SET(VCL_INCLUDE_DIR ${vxl_BINARY_DIR}/vcl ${vxl_SOURCE_DIR}/vcl)
SET(VCL_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/vcl)

# Autoconf does not work on windows.  Use hardcoded results if not using
# cmake to configure.
IF(VXL_NO_CMAKE_CONFIGURE)
  IF(WIN32)
    IF(NOT CYGWIN)
      SET(VCL_INCLUDE_DIR ${vxl_SOURCE_DIR}/vcl/config.win32 ${VCL_INCLUDE_DIR})
      SET(VCL_INSTALL_INCLUDE_DIR 
        ${CMAKE_INSTALL_PREFIX}/include/vcl/config.win32
        ${VCL_INSTALL_INCLUDE_DIR}
      )
    ENDIF(NOT CYGWIN)
  ENDIF(WIN32)
ENDIF(VXL_NO_CMAKE_CONFIGURE)

INCLUDE_DIRECTORIES(${VCL_INCLUDE_DIR} ${VXLCORE_INCLUDE_DIR})

# This SUBDIRS command must be the last SUBDIRS command in this file
SUBDIRS( config/cmake/export )
