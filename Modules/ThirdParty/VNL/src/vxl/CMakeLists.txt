#message(FATAL_ERROR "VXL has moved!
#VXL is now hosted in a GitHub repository at:
# https://github.com/vxl/vxl
#Run:
# git config --unset remote.origin.pushurl
# git remote set-url origin https://github.com/vxl/vxl.git
# git fetch origin
# git checkout master
# git reset --hard origin/master
#")
#
# Root vxl
#
#   vxl-maintainers@lists.sf.net
cmake_minimum_required(VERSION 2.8.9 FATAL_ERROR)

# Set policies for cmake
if( POLICY CMP0003 )
  cmake_policy(SET CMP0003 NEW)
endif()
# Use @rpath on OS X
if( POLICY CMP0042 )
  cmake_policy(SET CMP0042 NEW)
endif()

# Honor visibility properties for static libraries
if( POLICY CMP0063 )
  cmake_policy(SET CMP0063 NEW)
endif()

project(vxl)

include(CMakeDependentOption)


find_program( MEMORYCHECK_COMMAND valgrind )
if(MEMORYCHECK_COMMAND)
  set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --malloc-fill=0xFF" )
  set( MEMORYCHECK_SUPPRESSIONS_FILE "${CMAKE_CURRENT_LIST_DIR}/config/valgrind.supp" )
endif()

#-----------------------------------------------------------------------------
if(NOT COMMAND SETIFEMPTY)
  macro(SETIFEMPTY)
    set(KEY ${ARGV0})
    set(VALUE ${ARGV1})
    if(NOT ${KEY})
      set(${ARGV})
    endif()
  endmacro()
endif()


#-----------------------------------------------------------------------------
SETIFEMPTY(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
SETIFEMPTY(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
SETIFEMPTY(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)

#-----------------------------------------------------------------------------
SETIFEMPTY(CMAKE_INSTALL_LIBRARY_DESTINATION lib)
SETIFEMPTY(CMAKE_INSTALL_ARCHIVE_DESTINATION lib)
SETIFEMPTY(CMAKE_INSTALL_RUNTIME_DESTINATION bin)

# Allow external project to override the export target
if(NOT VXL_NO_EXPORT)
  SETIFEMPTY(VXL_INSTALL_EXPORT_NAME VXLTargets)
endif()

SETIFEMPTY(VXL_INSTALL_RUNTIME_DIR bin)
SETIFEMPTY(VXL_INSTALL_LIBRARY_DIR lib)
SETIFEMPTY(VXL_INSTALL_ARCHIVE_DIR lib)
SETIFEMPTY(VXL_INSTALL_INCLUDE_DIR include/vxl)
if(NOT VXL_LIB_PREFIX)
  set( VXL_LIB_PREFIX "") # This is typically empty
endif()

# CMake support directory.
set(VXL_ROOT_SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR})
set(VXL_CMAKE_DIR ${CMAKE_CURRENT_LIST_DIR}/config/cmake/Modules)

include( ${VXL_CMAKE_DIR}/VXLStandardOptions.cmake )
include( ${VXL_CMAKE_DIR}/VXLGenerateExportHeader.cmake )
include( ${CMAKE_CURRENT_LIST_DIR}/config/cmake/config/vxl_utils.cmake )
include(${CMAKE_CURRENT_LIST_DIR}/config/cmake/doxygen/doxygen.cmake)

# Location of VXL's FindXXX.cmake CMake modules.
# This is identical to VXL_CMAKE_DIR.  Perhaps we should eliminate MODULE_PATH?
set( MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/config/cmake/Modules CACHE STATIC "VXL module path" )

# Options to add extra compiler and linker flags
#
# These options allow you to specify additional flags without
# affecting the default flags for a particular platform or build type.
# This is especially useful for adding extra warning flags.
set( VXL_EXTRA_CMAKE_C_FLAGS             CACHE STRING "Extra flags appended to CMAKE_C_FLAGS" )
set( VXL_EXTRA_CMAKE_CXX_FLAGS           CACHE STRING "Extra flags appended to CMAKE_CXX_FLAGS" )
set( VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS    CACHE STRING "Extra flags appended to CMAKE_EXE_LINKER_FLAGS" )
set( VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS CACHE STRING "Extra flags appended to CMAKE_MODULE_LINKER_FLAGS" )
set( VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS CACHE STRING "Extra flags appended to CMAKE_SHARED_LINKER_FLAGS" )
set( CMAKE_C_FLAGS             "${CMAKE_C_FLAGS} ${VXL_EXTRA_CMAKE_C_FLAGS}" )
set( CMAKE_CXX_FLAGS           "${CMAKE_CXX_FLAGS} ${VXL_EXTRA_CMAKE_CXX_FLAGS}" )
set( CMAKE_EXE_LINKER_FLAGS    "${CMAKE_EXE_LINKER_FLAGS} ${VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS}" )
set( CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS}" )
set( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS}" )


#-------------------------------------------------------------------
#-- BUILD CONFIG OPTIONS

# Optionally use old error reporting methods, rather than exceptions.
# The main use is in vil which often uses/used null images to imply an error.
option(VXL_LEGACY_ERROR_REPORTING "Use old error reporting methods rather than exceptions?" ON)
if(VXL_LEGACY_ERROR_REPORTING)
  add_definitions( -DVXL_LEGACY_ERROR_REPORTING )
endif()

# Option to build Windows Unicode support, the string
# type of which is wchar_t, each character is a 16-bit unsigned integer.
if(WIN32)
  if(VXL_HAS_WIN_WCHAR_T)
    option( VXL_USE_WIN_WCHAR_T "Build overloading functions that accept Windows wide char strings?" ON )
    if(VXL_USE_WIN_WCHAR_T)   # Force it to be 0/1
      set(VXL_USE_WIN_WCHAR_T 1)
    else()
      set(VXL_USE_WIN_WCHAR_T 0)
    endif()
  else()
    set(VXL_USE_WIN_WCHAR_T 0)
  endif()
else()
  # avoid empty macro definition
  set(VXL_USE_WIN_WCHAR_T 0)
endif()

# In order to be able to link vxl libraries into shared libraries on 64 bit linux, the -fPIC
# compiler flag must be added.  Only do this if we are on a x86_64 *nix platform, we're building
# static libraries, and the user has not explicitly requested position dependent code.
if(UNIX)
  if(NOT VXL_BUILD_SHARED_LIBS AND CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64")
    option(BUILD_POSITION_DEPENDENT_CODE "Generate position dependent code (i.e. code cannot be used in shared library)" OFF)
    mark_as_advanced(BUILD_POSITION_DEPENDENT_CODE)
    if(NOT BUILD_POSITION_DEPENDENT_CODE)
      message(STATUS "Adding -fPIC compiler flag to generate position independent code.")
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
    endif()
  endif()
endif()


# Some types of path names can cause havoc with regular expressions,
# so avoid those.
if( ${PROJECT_BINARY_DIR} MATCHES  \\+ )
  message(SEND_ERROR "You cannot have a + in your binary path")
endif()
if( ${CMAKE_CURRENT_LIST_DIR} MATCHES  \\+ )
  message(SEND_ERROR "You cannot have a + in your source path")
endif()

# include CMakeListsLocal.txt from source directory if it exists
# also include it from the binary dir if different from source dir
if( ${PROJECT_BINARY_DIR} MATCHES ${CMAKE_CURRENT_LIST_DIR} )
  include( ${CMAKE_CURRENT_LIST_DIR}/CMakeListsLocal.txt OPTIONAL )
else()
  include( ${CMAKE_CURRENT_LIST_DIR}/CMakeListsLocal.txt OPTIONAL )
  include( ${PROJECT_BINARY_DIR}/CMakeListsLocal.txt OPTIONAL )
endif()

# Standard include directories.
set(VXLCORE_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/core)    #Source includes
set(VXLCORE_BINARY_INCLUDE_DIR ${PROJECT_BINARY_DIR}/core) #Generated includes
set(VXLCORE_INCLUDE_DIRS ${VXLCORE_BINARY_INCLUDE_DIR} ${VXLCORE_INCLUDE_DIR})
set(VXLCORE_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/core)

set(VCL_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/vcl)    #Source includes
set(VCL_BINARY_INCLUDE_DIR ${PROJECT_BINARY_DIR}/vcl) #Generated includes
set(VCL_INCLUDE_DIRS ${VCL_BINARY_INCLUDE_DIR} ${VCL_INCLUDE_DIR})
set(VCL_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/vcl)

include_directories(${VCL_INCLUDE_DIRS} ${VXLCORE_INCLUDE_DIRS})

# Do platform-specific configuration.
include(${CMAKE_CURRENT_LIST_DIR}/config/cmake/config/VXLIntrospectionConfig.cmake)

#-------------------------------------------------------------------
# This block should come after VXLIntrospectionConfig.cmake has been executed.
if(VCL_HAS_LFS OR WIN32)
  option( VXL_USE_LFS "Should VXL use Large File Support?" NO)
  mark_as_advanced( VXL_USE_LFS )
endif()

if(VXL_USE_LFS)
  if(WIN32)
    # TODO: MS Version Support
    #  message( SEND_ERROR "Sorry - Large File Support is not quite working on Win32 yet. Turning VXL_USE_LFS off")
    #  set(VXL_USE_LFS "NO" CACHE BOOL "Should VXL use Large File Support?" FORCE)
  else()
    if(VCL_HAS_LFS)
      add_definitions( -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE)
    else()
      message( SEND_ERROR "This platform does not have Large File Support - turning VXL_USE_LFS off")
      set(VXL_USE_LFS "NO" CACHE BOOL "Should VXL use Large File Support?" FORCE)
    endif()
  endif()
endif()

#-------------------------------------------------------------------
#-- BUILD SELECTION OPTIONS
# Options for selecting the core core libraries
option( BUILD_CORE_GEOMETRY "Build VXL's geometry libraries" ON )
option( BUILD_CORE_NUMERICS "Build VXL's numerics libraries" ON )
option( BUILD_CORE_SERIALISATION "Build VXL's serialisation libraries" ON )
option( BUILD_CORE_UTILITIES "Build VXL's utility libraries" ON )
option( BUILD_CORE_IMAGING "Build VXL's imaging libraries" ON )

# By default, build examples when building tests. Examples require most of the core libraries
CMAKE_DEPENDENT_OPTION( BUILD_EXAMPLES "Should the examples be built?" ${BUILD_TESTING}
                      "BUILD_CORE_GEOMETRY;BUILD_CORE_NUMERICS;BUILD_CORE_UTILITIES;BUILD_CORE_SERIALISATION;BUILD_CORE_IMAGING" OFF)

# Option to specify whether this is a build for the dashboard.  Each
# dashboard build should set BUILD_FOR_VXL_DASHBOARD to ON in the
# initial cache (set in the CTest script).
option( BUILD_FOR_VXL_DASHBOARD "Enable deprecated code and known failing test?)" OFF )
option( BUILD_NONDEPRECATED_ONLY "Build only nondeprecated libraries (Experimental)" ON )

CMAKE_DEPENDENT_OPTION(BUILD_CORE_PROBABILITY "Build VXL's probability libraries (Experimental)" ON "BUILD_CORE_NUMERICS;BUILD_FOR_VXL_DASHBOARD" OFF)

# Build the core vxl + support libraries
add_subdirectory(vcl)
add_subdirectory(v3p)
add_subdirectory(core)

# Optionally build the contributed libraries
if( EXISTS ${CMAKE_CURRENT_LIST_DIR}/contrib/CMakeLists.txt )
  CMAKE_DEPENDENT_OPTION(BUILD_CONTRIB "Build the contributed libraries?" ON "BUILD_CORE_GEOMETRY;BUILD_CORE_NUMERICS;BUILD_CORE_UTILITIES;BUILD_CORE_SERIALISATION;BUILD_CORE_IMAGING" OFF)
  add_subdirectory(contrib)
endif()

# Generate Project files dependacy generation for now for downstream packages

# Copy the UseVXL.cmake file to the binary directory so that client
# projects don't need to find the source directory first. They can run
# the UseVXL.cmake from the vxl binary directory, and determine the
# vxl source directory by loading the cache.
configure_file( ${VXL_CMAKE_DIR}/UseVXL.cmake
                ${vxl_BINARY_DIR}/UseVXL.cmake COPYONLY )
# For use in client projects that call UseVXL.cmake
set(VXL_LIBRARY_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} CACHE STATIC "Where all the vxl libraries are, for clients to use." )

# Copy CTestCustom.cmake to top of build tree
configure_file( ${VXL_CMAKE_DIR}/CTestCustom.cmake
  ${vxl_BINARY_DIR}/CTestCustom.cmake COPYONLY )

# This command must be the last command in this file
if(NOT VXL_NO_EXPORT)
  include(${CMAKE_CURRENT_LIST_DIR}/config/cmake/export/VXLCreateProject.cmake)
endif()
