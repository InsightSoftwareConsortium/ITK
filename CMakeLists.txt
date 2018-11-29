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

cmake_minimum_required(VERSION 3.10.2 FATAL_ERROR)
if( "${CMAKE_CXX_STANDARD}" MATCHES "(11|14|17|20)")
   # If building for modern C++ language standards,
   # require use of newer cmake version
else()
   set(CMAKE_CXX_STANDARD "11")
endif()
if(NOT CMAKE_CXX_STANDARD_REQUIRED)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()
if(NOT CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)
endif()

# Set policies consistent with newer versions of cmake
# to ease integration with projects that require newer
# cmake versions.
foreach(p
    ## Only policies introduced after the cmake_minimum_required
    ## version need to explicitly be set to NEW.

    ##----- Policies Introduced by CMake 3.12
    CMP0075  #: Include file check macros honor CMAKE_REQUIRED_LIBRARIES.
    ##----- Policies Introduced by CMake 3.10
    CMP0071  #: Let AUTOMOC and AUTOUIC process GENERATED files.
    CMP0070  #: Define file(GENERATE) behavior for relative paths.
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

project(VXL #Project name must be all caps to have properly generated VXL_VERSION_* variables
    VERSION 2.0.2.0
    DESCRIPTION "A multi-platform collection of C++ software libraries for Computer Vision and Image Understanding."
    LANGUAGES CXX C)

include(CMakeDependentOption)
include(${CMAKE_CURRENT_SOURCE_DIR}/config/cmake/VXLInitializeBuildType.cmake)

#Disable overzealous compiler warning.  If the definition is truely missing a link error will be created.
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG(-Wno-undefined-var-template HAS_NO_UNDEFINED_VAR_TEMPLATE)
if( HAS_NO_UNDEFINED_VAR_TEMPLATE )
  add_definitions( -Wno-undefined-var-template )
endif()

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
include( GenerateExportHeader )
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
option( VXL_LEGACY_FUTURE_REMOVE "Disable features and cause errors for code deprecated, or listed for removal" ON)
mark_as_advanced(VXL_LEGACY_FUTURE_REMOVE)
option(VXL_RUN_FAILING_TESTS "Enable long-time failing tests. If tests are failing for a long time, turn them off by default." OFF)
mark_as_advanced(VXL_RUN_FAILING_TESTS)

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
  if(NOT BUILD_SHARED_LIBS AND CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64")
    option(VXL_BUILD_POSITION_DEPENDENT_CODE "Generate position dependent code (i.e. code cannot be used in shared library)" OFF)
    mark_as_advanced(VXL_BUILD_POSITION_DEPENDENT_CODE)
    if(NOT VXL_BUILD_POSITION_DEPENDENT_CODE)
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
option( VXL_BUILD_CORE_NUMERICS_ONLY "Turn off everything except the core numerics" OFF)
mark_as_advanced(VXL_BUILD_CORE_NUMERICS_ONLY)
option( VXL_BUILD_CORE_NUMERICS "Build VXL's numerics libraries" ON )
CMAKE_DEPENDENT_OPTION( VXL_BUILD_CORE_GEOMETRY "Build VXL's geometry libraries" ON "NOT VXL_BUILD_CORE_NUMERICS_ONLY" OFF)
CMAKE_DEPENDENT_OPTION( VXL_BUILD_CORE_SERIALISATION "Build VXL's serialisation libraries" ON "NOT VXL_BUILD_CORE_NUMERICS_ONLY" OFF)
CMAKE_DEPENDENT_OPTION( VXL_BUILD_CORE_UTILITIES "Build VXL's utility libraries" ON "NOT VXL_BUILD_CORE_NUMERICS_ONLY" OFF)
CMAKE_DEPENDENT_OPTION( VXL_BUILD_CORE_IMAGING "Build VXL's imaging libraries" ON "NOT VXL_BUILD_CORE_NUMERICS_ONLY" OFF)

# By default, build examples when building tests. Examples require most of the core libraries
CMAKE_DEPENDENT_OPTION( VXL_BUILD_EXAMPLES "Should the examples be built?" ${BUILD_TESTING}
                      "VXL_BUILD_CORE_GEOMETRY;VXL_BUILD_CORE_NUMERICS;VXL_BUILD_CORE_UTILITIES;VXL_BUILD_CORE_SERIALISATION;VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced(VXL_BUILD_EXAMPLES)

option( VXL_BUILD_NONDEPRECATED_ONLY "Build only nondeprecated libraries (Experimental)" ON )
mark_as_advanced(VXL_BUILD_NONDEPRECATED_ONLY)

CMAKE_DEPENDENT_OPTION(VXL_BUILD_CORE_PROBABILITY "Build VXL's probability libraries (Experimental)" ON
                       "VXL_BUILD_CORE_NUMERICS;NOT VXL_BUILD_CORE_NUMERICS_ONLY" OFF)

# Build the core vxl + support libraries
add_subdirectory(vcl)
add_subdirectory(v3p)
add_subdirectory(core)

# Optionally build the contributed libraries
if( EXISTS ${CMAKE_CURRENT_LIST_DIR}/contrib/CMakeLists.txt )
  CMAKE_DEPENDENT_OPTION(VXL_BUILD_CONTRIB "Build the contributed libraries?" OFF "VXL_BUILD_CORE_GEOMETRY;VXL_BUILD_CORE_NUMERICS;VXL_BUILD_CORE_UTILITIES;VXL_BUILD_CORE_SERIALISATION;VXL_BUILD_CORE_IMAGING;NOT VXL_BUILD_CORE_NUMERICS_ONLY" OFF)
  add_subdirectory(contrib)
endif()

# Generate Project files dependacy generation for now for downstream packages

# Copy the UseVXL.cmake file to the binary directory so that client
# projects don't need to find the source directory first. They can run
# the UseVXL.cmake from the vxl binary directory, and determine the
# vxl source directory by loading the cache.
configure_file( ${VXL_CMAKE_DIR}/UseVXL.cmake
                ${VXL_BINARY_DIR}/UseVXL.cmake COPYONLY )
# For use in client projects that call UseVXL.cmake
set(VXL_LIBRARY_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} CACHE STATIC "Where all the vxl libraries are, for clients to use." )

# Copy CTestCustom.cmake to top of build tree
configure_file( ${VXL_CMAKE_DIR}/CTestCustom.cmake
  ${VXL_BINARY_DIR}/CTestCustom.cmake COPYONLY )

# This command must be the last command in this file
if(NOT VXL_NO_EXPORT)
  include(${CMAKE_CURRENT_LIST_DIR}/config/cmake/export/VXLCreateProject.cmake)
endif()

option(VXL_BUILD_OBJECT_LIBRARIES "Build object libraries (to enable all in a single library)?" OFF)
mark_as_advanced(VXL_BUILD_OBJECT_LIBRARIES)
