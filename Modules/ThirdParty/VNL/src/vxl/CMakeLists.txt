# Root vxl

cmake_minimum_required(VERSION 3.22.1...3.31.9)

# Default the C++/C standards; the per-target C++ requirement is expressed
# with target_compile_features(... cxx_std_${CMAKE_CXX_STANDARD}) in
# vxl_add_library, which propagates to consumers.
if(NOT CMAKE_CXX_STANDARD)
   set(CMAKE_CXX_STANDARD "17")
endif()
if(NOT CMAKE_CXX_STANDARD_REQUIRED)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()
if(NOT CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)
endif()

if(NOT CMAKE_C_STANDARD)
   set(CMAKE_C_STANDARD "11")
endif()
if(NOT CMAKE_C_STANDARD_REQUIRED)
  set(CMAKE_C_STANDARD_REQUIRED ON)
endif()
if(NOT CMAKE_C_EXTENSIONS)
  set(CMAKE_C_EXTENSIONS OFF)
endif()

project(VXL #Project name must be all caps to have properly generated VXL_VERSION_* variables
    VERSION 7.0.0.0 # defines #MAJOR,MINOR,PATCH,TWEAK}; major bump signals C++17 minimum
    DESCRIPTION "A multi-platform collection of C++ software libraries for Computer Vision and Image Understanding."
    LANGUAGES CXX C)

SET(VXL_API_VERSION "${VXL_VERSION_MAJOR}.${VXL_VERSION_MINOR}")
SET(VXL_LIBRARY_PROPERTIES ${VXL_LIBRARY_PROPERTIES} VERSION "${VXL_VERSION}"
    SOVERSION "${VXL_API_VERSION}")

include(CMakeDependentOption)

#Disable overzealous compiler warning.  If the definition is truely missing a link error will be created.
include(CheckCXXCompilerFlag)
include(CheckPIESupported)
check_pie_supported()

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
include(GNUInstallDirs)
SETIFEMPTY(VXL_INSTALL_RUNTIME_DIR ${CMAKE_INSTALL_BINDIR})
SETIFEMPTY(VXL_INSTALL_LIBRARY_DIR ${CMAKE_INSTALL_LIBDIR})
SETIFEMPTY(VXL_INSTALL_ARCHIVE_DIR ${CMAKE_INSTALL_LIBDIR})
SETIFEMPTY(VXL_INSTALL_INCLUDE_DIR include/vxl)
# Export-set name for install(TARGETS ... EXPORT ...). ITK overrides this
# to fold the vxl targets into its own export set; standalone builds use it
# as a local export set name (no standalone VXLConfig is generated).
SETIFEMPTY(VXL_INSTALL_EXPORT_NAME VXLTargets)

# CMake support directory.
set(VXL_ROOT_SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR})
set(VXL_CMAKE_DIR ${CMAKE_CURRENT_LIST_DIR}/config/cmake/Modules)

include( ${VXL_CMAKE_DIR}/VXLStandardOptions.cmake )
include( GenerateExportHeader )
include( ${CMAKE_CURRENT_LIST_DIR}/config/cmake/config/vxl_utils.cmake )
# probe VXL_PLATFORM_MATH_LIBRARIES (system math libs needed to link)
include(${CMAKE_CURRENT_LIST_DIR}/config/cmake/Modules/ConfigurePlatformMathTarget.cmake)


if(MSVC)
  # Force synchronous writes of .pdb files (/FS) to avoid PDB contention.
  add_compile_options( "/FS" )
endif()
#-------------------------------------------------------------------
#-- BUILD CONFIG OPTIONS

# Legacy capability switches. When embedded in ITK these are already defined by
# the host and its values are honored; for standalone builds default to keeping
# legacy compatibility (e.g. the backward-compat vcl static archive).
if(NOT DEFINED ITK_LEGACY_REMOVE)
  option(ITK_LEGACY_REMOVE "Remove all legacy backward-compatibility code." OFF)
  mark_as_advanced(ITK_LEGACY_REMOVE)
endif()
if(NOT DEFINED ITK_FUTURE_LEGACY_REMOVE)
  option(ITK_FUTURE_LEGACY_REMOVE "Remove code scheduled to become legacy in a future release." OFF)
  mark_as_advanced(ITK_FUTURE_LEGACY_REMOVE)
endif()

# Build position-independent code so the static vxl libraries can be linked
# into shared libraries (as ITK does). CMake emits the correct per-compiler
# flag across platforms.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Standard include directories.
set(VXLCORE_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/core)    #Source includes
set(VXLCORE_BINARY_INCLUDE_DIR ${PROJECT_BINARY_DIR}/core) #Generated includes
set(VXLCORE_INCLUDE_DIRS ${VXLCORE_BINARY_INCLUDE_DIR} ${VXLCORE_INCLUDE_DIR})
set(VXLCORE_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/core)

set(VCL_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/vcl)    #Source includes
set(VCL_BINARY_INCLUDE_DIR ${PROJECT_BINARY_DIR}/vcl) #Generated includes
set(VCL_INCLUDE_DIRS ${VCL_BINARY_INCLUDE_DIR} ${VCL_INCLUDE_DIR})
set(VCL_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/vcl)

# Record the C++ standard VXL was built with as its __cplusplus value, so
# vxl_config.h can warn when a consumer compiles against an older standard.
# The floor is C++17 (enforced above), so map the standard level directly.
if(CMAKE_CXX_STANDARD GREATER_EQUAL 23)
  set(VXL_COMPILED_CXX_STANDARD_VERSION 202302L)
elseif(CMAKE_CXX_STANDARD GREATER_EQUAL 20)
  set(VXL_COMPILED_CXX_STANDARD_VERSION 202002L)
else()
  set(VXL_COMPILED_CXX_STANDARD_VERSION 201703L)
endif()


#-------------------------------------------------------------------
# Build the core vxl + support libraries
add_subdirectory(vcl)
add_subdirectory(v3p)
add_subdirectory(core)

# Copy CTestCustom.cmake to top of build tree
configure_file( ${VXL_CMAKE_DIR}/CTestCustom.cmake
  ${VXL_BINARY_DIR}/CTestCustom.cmake COPYONLY )
