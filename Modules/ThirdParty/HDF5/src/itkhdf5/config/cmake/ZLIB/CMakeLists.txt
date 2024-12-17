cmake_minimum_required (VERSION 3.18)
set (CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS ON)
project (ZLIB C)

set (VERSION "1.3")

#-----------------------------------------------------------------------------
# Basic ZLIB stuff here
#-----------------------------------------------------------------------------
set (CMAKE_POSITION_INDEPENDENT_CODE ON)

set (ZLIB_PACKAGE_EXT ${HDF_PACKAGE_EXT})
set (HDF_USE_GNU_DIRS ${HDF5_USE_GNU_DIRS})
set (CMAKE_OSX_ARCHITECTURES ${CMAKE_OSX_ARCHITECTURES})
set (CMAKE_TOOLCHAIN_FILE ${CMAKE_TOOLCHAIN_FILE})
set (PACKAGE_NAMESPACE ${HDF_PACKAGE_NAMESPACE})
if (MINGW)
  set (WINDOWS 1) # MinGW tries to imitate Windows
endif ()
if (WINDOWS)
  set (HAVE_STDDEF_H 1)
  set (HAVE_SYS_TYPES_H 1)
endif ()

include (CheckTypeSize)
include (CheckFunctionExists)
include (CheckIncludeFile)
include (CheckCSourceCompiles)

check_include_file (sys/types.h HAVE_SYS_TYPES_H)
check_include_file (stdint.h    HAVE_STDINT_H)
check_include_file (stddef.h    HAVE_STDDEF_H)

#
# Check to see if we have large file support
#
set (CMAKE_REQUIRED_DEFINITIONS -D_LARGEFILE64_SOURCE=1)
# We add these other definitions here because CheckTypeSize.cmake
# in CMake 2.4.x does not automatically do so and we want
# compatibility with CMake 2.4.x.
if (HAVE_SYS_TYPES_H)
  list(APPEND CMAKE_REQUIRED_DEFINITIONS -DHAVE_SYS_TYPES_H)
endif ()
if (HAVE_STDINT_H)
  list(APPEND CMAKE_REQUIRED_DEFINITIONS -DHAVE_STDINT_H)
endif ()
if (HAVE_STDDEF_H)
  list(APPEND CMAKE_REQUIRED_DEFINITIONS -DHAVE_STDDEF_H)
endif ()
check_type_size (off64_t OFF64_T)
if (HAVE_OFF64_T)
  add_definitions(-D_LARGEFILE64_SOURCE=1)
endif ()
set (CMAKE_REQUIRED_DEFINITIONS) # clear variable

#
# Check for fseeko
#
check_function_exists (fseeko HAVE_FSEEKO)
if (NOT HAVE_FSEEKO)
  add_definitions (-DNO_FSEEKO)
endif ()

#
# Check for unistd.h
#
check_include_file (unistd.h Z_HAVE_UNISTD_H)

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (ZLIB_RESOURCES_DIR          ${HDF_RESOURCES_DIR}/ZLIB)
set (ZLIB_SRC_DIR                ${ZLIB_SOURCE_DIR})

#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
set (ZLIB_LIB_CORENAME            "zlib-static")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
set (ZLIB_LIB_NAME            "${ZLIB_EXTERNAL_LIB_PREFIX}${ZLIB_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
set (ZLIB_LIB_TARGET            "zlib-static")

# parse the full version number from zlib.h and include in ZLIB_FULL_VERSION
file (READ ${CMAKE_CURRENT_SOURCE_DIR}/zlib.h _zlib_h_contents)
string (REGEX REPLACE ".*#define[ \t]+ZLIB_VERSION[ \t]+\"([-0-9A-Za-z.]+)\".*"
    "\\1" ZLIB_FULL_VERSION ${_zlib_h_contents})

set (zlib_VERS_MAJOR 1)
set (zlib_VERS_MINOR 3)
set (zlib_VERS_RELEASE 0)

#-----------------------------------------------------------------------------
set (ZLIB_PACKAGE "zlib")
set (ZLIB_PACKAGE_NAME "ZLIB")
set (ZLIB_PACKAGE_VERSION "${zlib_VERS_MAJOR}.${zlib_VERS_MINOR}")
set (ZLIB_PACKAGE_VERSION_MAJOR "${zlib_VERS_MAJOR}.${zlib_VERS_MINOR}")
set (ZLIB_PACKAGE_VERSION_MINOR "${zlib_VERS_RELEASE}")
set (ZLIB_PACKAGE_STRING "${ZLIB_PACKAGE_NAME} ${ZLIB_PACKAGE_VERSION}")
set (ZLIB_PACKAGE_TARNAME "${ZLIB_PACKAGE_NAME}${ZLIB_PACKAGE_EXT}")
set (ZLIB_PACKAGE_URL "http://www.hdfgroup.org")
set (ZLIB_PACKAGE_BUGREPORT "help@hdfgroup.org")
set (ZLIB_PACKAGE_SOVERSION "${zlib_VERS_MAJOR}.${zlib_VERS_MINOR}.${zlib_VERS_RELEASE}")
set (ZLIB_PACKAGE_SOVERSION_MAJOR "${zlib_VERS_MAJOR}")

HDF_DIR_PATHS(${ZLIB_PACKAGE_NAME})

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXPORTED_TARGETS)
  set (ZLIB_EXPORTED_TARGETS "zlib-targets")
endif ()

set_global_variable (ZLIB_LIBRARIES_TO_EXPORT "")

#-----------------------------------------------------------------------------
# All libs/tests/examples need the main include directories
#-----------------------------------------------------------------------------
set_directory_properties (PROPERTIES INCLUDE_DIRECTORIES
    "${ZLIB_BINARY_DIR};${ZLIB_SOURCE_DIR};${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
)

if (MSVC)
  set(CMAKE_DEBUG_POSTFIX "d")
  add_definitions (-D_BIND_TO_CURRENT_VCLIBS_VERSION=1)
  add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  add_definitions (-D_CONSOLE)
  add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)
  include_directories(${CMAKE_CURRENT_SOURCE_DIR})
endif ()

if(NOT CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_CURRENT_BINARY_DIR)
    # If we're doing an out of source build and the user has a zconf.h
    # in their source tree...
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/zconf.h)
        message(STATUS "Renaming")
        message(STATUS "    ${CMAKE_CURRENT_SOURCE_DIR}/zconf.h")
        message(STATUS "to 'zconf.h.included' because this file is included with zlib")
        message(STATUS "but CMake generates it automatically in the build directory.")
        file(RENAME ${CMAKE_CURRENT_SOURCE_DIR}/zconf.h ${CMAKE_CURRENT_SOURCE_DIR}/zconf.h.included)
  endif()
endif()
configure_file(	${CMAKE_CURRENT_SOURCE_DIR}/zconf.h.cmakein
		${CMAKE_CURRENT_BINARY_DIR}/zconf.h @ONLY)
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR})


#-----------------------------------------------------------------------------
# Define ZLIB Library
#-----------------------------------------------------------------------------

set(ZLIB_PUBLIC_HDRS
    ${CMAKE_CURRENT_BINARY_DIR}/zconf.h
    zlib.h
)
set(ZLIB_PRIVATE_HDRS
    crc32.h
    deflate.h
    gzguts.h
    inffast.h
    inffixed.h
    inflate.h
    inftrees.h
    trees.h
    zutil.h
)
set(ZLIB_SRCS
    adler32.c
    compress.c
    crc32.c
    deflate.c
    gzclose.c
    gzlib.c
    gzread.c
    gzwrite.c
    inflate.c
    infback.c
    inftrees.c
    inffast.c
    trees.c
    uncompr.c
    zutil.c
)

add_library(${ZLIB_LIB_TARGET} STATIC ${ZLIB_SRCS} ${ZLIB_PUBLIC_HDRS} ${ZLIB_PRIVATE_HDRS})
target_include_directories(${ZLIB_LIB_TARGET} PRIVATE "${CMAKE_BINARY_DIR}")
if (MSVC AND CMAKE_CL_64)
  set_target_properties (${ZLIB_LIB_TARGET} PROPERTIES STATIC_LIBRARY_FLAGS "/machine:x64")
endif ()
set_target_properties(${ZLIB_LIB_TARGET} PROPERTIES
    PUBLIC_HEADER ""
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

#-----------------------------------------------------------------------------
# Compiler specific flags
#-----------------------------------------------------------------------------
if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
  target_compile_options(${ZLIB_LIB_TARGET} PRIVATE -Wno-strict-prototypes -Wno-implicit-function-declaration)
endif ()
if (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
  target_compile_options(${ZLIB_LIB_TARGET} PRIVATE -Wno-implicit-function-declaration)
endif ()
if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
  target_compile_options(${ZLIB_LIB_TARGET} PRIVATE -fmessage-length=0)
endif ()

set_target_properties(${ZLIB_LIB_TARGET} PROPERTIES OUTPUT_NAME zlib-static)

set_global_variable (ZLIB_LIBRARIES_TO_EXPORT "${ZLIB_LIB_TARGET}")
set (install_targets ${ZLIB_LIB_TARGET})

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (ZLIB_EXPORTED_TARGETS)
  INSTALL_TARGET_PDB (${ZLIB_LIB_TARGET} ${ZLIB_INSTALL_BIN_DIR} libraries)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${ZLIB_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${ZLIB_INSTALL_LIB_DIR} COMPONENT libraries
      ARCHIVE DESTINATION ${ZLIB_INSTALL_LIB_DIR} COMPONENT libraries
      RUNTIME DESTINATION ${ZLIB_INSTALL_BIN_DIR} COMPONENT libraries
      FRAMEWORK DESTINATION ${ZLIB_INSTALL_FWRK_DIR} COMPONENT libraries
      PUBLIC_HEADER DESTINATION ${ZLIB_INSTALL_INCLUDE_DIR} COMPONENT headers
  )
endif ()

include (CMakePackageConfigHelpers)

#-----------------------------------------------------------------------------
# Configure the zlib-config.cmake file for the build directory
#-----------------------------------------------------------------------------
set (INCLUDE_INSTALL_DIR ${ZLIB_INSTALL_INCLUDE_DIR})
set (SHARE_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/${ZLIB_INSTALL_CMAKE_DIR}" )
set (CURRENT_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}" )
configure_package_config_file (
    ${ZLIB_RESOURCES_DIR}/zlib-config.cmake.in
    "${ZLIB_BINARY_DIR}/zlib-config.cmake"
    INSTALL_DESTINATION "${ZLIB_INSTALL_CMAKE_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR SHARE_INSTALL_DIR CURRENT_BUILD_DIR
    INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
)

#-----------------------------------------------------------------------------
# Configure the zlib-config.cmake file for the install directory
#-----------------------------------------------------------------------------
set (INCLUDE_INSTALL_DIR ${ZLIB_INSTALL_INCLUDE_DIR})
set (SHARE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${ZLIB_INSTALL_CMAKE_DIR}" )
set (CURRENT_BUILD_DIR "${CMAKE_INSTALL_PREFIX}")
configure_package_config_file (
    ${ZLIB_RESOURCES_DIR}/zlib-config.cmake.in
    "${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/zlib-config.cmake"
    INSTALL_DESTINATION "${ZLIB_INSTALL_CMAKE_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR SHARE_INSTALL_DIR CURRENT_BUILD_DIR
)
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  install (
      FILES ${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/zlib-config.cmake
      DESTINATION ${ZLIB_INSTALL_CMAKE_DIR}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Configure the ZLIB-config-version.cmake file for the install directory
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  configure_file (
      ${ZLIB_RESOURCES_DIR}/zlib-config-version.cmake.in
      ${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/zlib-config-version.cmake @ONLY
  )
  install (
      FILES ${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/zlib-config-version.cmake
      DESTINATION ${ZLIB_INSTALL_CMAKE_DIR}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  install (
      EXPORT ${ZLIB_EXPORTED_TARGETS}
      DESTINATION ${ZLIB_INSTALL_CMAKE_DIR}
      FILE zlib-targets.cmake
      NAMESPACE ${PACKAGE_NAMESPACE}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Export all exported targets to the build tree for use by parent project
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  export (
      TARGETS ${ZLIB_LIBRARIES_TO_EXPORT}
      FILE zlib-targets.cmake
      NAMESPACE ${PACKAGE_NAMESPACE}
  )
  export (PACKAGE zlib)
endif ()

