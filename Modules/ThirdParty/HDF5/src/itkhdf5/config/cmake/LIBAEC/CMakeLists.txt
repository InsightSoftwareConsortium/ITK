cmake_minimum_required (VERSION 3.18)
project (LIBAEC C)

#-----------------------------------------------------------------------------
# Basic LIBAEC stuff here
#-----------------------------------------------------------------------------
set (CMAKE_C_STANDARD 99)
set (CMAKE_C_VISIBILITY_PRESET hidden)
set (CMAKE_POSITION_INDEPENDENT_CODE ON)

set (LIBAEC_PACKAGE_EXT ${HDF_PACKAGE_EXT})
set (HDF_USE_GNU_DIRS ${HDF5_USE_GNU_DIRS})
set (CMAKE_OSX_ARCHITECTURES ${CMAKE_OSX_ARCHITECTURES})
set (CMAKE_TOOLCHAIN_FILE ${CMAKE_TOOLCHAIN_FILE})
set (PACKAGE_NAMESPACE ${HDF_PACKAGE_NAMESPACE})
if (MINGW)
  set (WINDOWS 1) # MinGW tries to imitate Windows
endif ()
if (WINDOWS)
  set (HAVE_SYS_STAT_H 1)
  set (HAVE_SYS_TYPES_H 1)
endif ()

if (NOT WINDOWS)
  include (TestBigEndian)
  test_big_endian (WORDS_BIGENDIAN)
endif ()

# Check for __builtin_clzll for faster decoding
include (CheckCSourceCompiles)
check_c_source_compiles(
  "int main(void)\n{return __builtin_clzll(1LL);}"
  HAVE_DECL___BUILTIN_CLZLL)

if (NOT HAVE_DECL___BUILTIN_CLZLL)
  # With MSVC we can use _BitScanReverse64
  check_c_source_compiles (
    "int main(void){unsigned long foo; unsigned __int64 bar=1LL;
return _BitScanReverse64(&foo, bar);}"
    HAVE_BSR64)
endif ()

include (CheckSymbolExists)
check_symbol_exists (snprintf "stdio.h" HAVE_SNPRINTF)
if (NOT HAVE_SNPRINTF)
  check_symbol_exists (_snprintf "stdio.h" HAVE__SNPRINTF)
  check_symbol_exists (_snprintf_s "stdio.h" HAVE__SNPRINTF_S)
endif ()

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (LIBAEC_RESOURCES_DIR          ${HDF_RESOURCES_DIR}/LIBAEC)
set (LIBAEC_SRC_DIR                ${LIBAEC_SOURCE_DIR}/src)
set (LIBAEC_INC_DIR                ${LIBAEC_SOURCE_DIR}/include)

#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
set (LIBAEC_LIB_CORENAME            "aec")
set (SZIP_LIB_CORENAME              "szaec")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
set (LIBAEC_LIB_NAME            "${LIBAEC_EXTERNAL_LIB_PREFIX}${LIBAEC_LIB_CORENAME}")
set (SZIP_LIB_NAME              "${LIBAEC_EXTERNAL_LIB_PREFIX}${SZIP_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
set (LIBAEC_LIB_TARGET            "${LIBAEC_LIB_CORENAME}-static")
set (SZIP_LIB_TARGET              "${SZIP_LIB_CORENAME}-static")

set (libaec_VERS_MAJOR 1)
set (libaec_VERS_MINOR 1)
set (libaec_VERS_RELEASE 3)

#-----------------------------------------------------------------------------
set (LIBAEC_PACKAGE "libaec")
set (LIBAEC_PACKAGE_NAME "LIBAEC")
set (LIBAEC_PACKAGE_VERSION "${libaec_VERS_MAJOR}.${libaec_VERS_MINOR}")
set (LIBAEC_PACKAGE_VERSION_MAJOR "${libaec_VERS_MAJOR}.${libaec_VERS_MINOR}")
set (LIBAEC_PACKAGE_VERSION_MINOR "${libaec_VERS_RELEASE}")
set (LIBAEC_PACKAGE_STRING "${LIBAEC_PACKAGE_NAME} ${LIBAEC_PACKAGE_VERSION}")
set (LIBAEC_PACKAGE_TARNAME "${LIBAEC_PACKAGE_NAME}${LIBAEC_PACKAGE_EXT}")
set (LIBAEC_PACKAGE_URL "http://www.hdfgroup.org")
set (LIBAEC_PACKAGE_BUGREPORT "help@hdfgroup.org")
set (LIBAEC_PACKAGE_SOVERSION "${libaec_VERS_MAJOR}.${libaec_VERS_MINOR}.${libaec_VERS_RELEASE}")
set (LIBAEC_PACKAGE_SOVERSION_MAJOR "${libaec_VERS_MAJOR}")

HDF_DIR_PATHS(${LIBAEC_PACKAGE_NAME})

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects
#-----------------------------------------------------------------------------
if (NOT LIBAEC_EXPORTED_TARGETS)
  set (LIBAEC_EXPORTED_TARGETS "libaec-targets")
endif ()

set_global_variable (LIBAEC_LIBRARIES_TO_EXPORT "")

#-----------------------------------------------------------------------------
# All libs/tests/examples need the main include directories
#-----------------------------------------------------------------------------
set_directory_properties (PROPERTIES INCLUDE_DIRECTORIES
    "${LIBAEC_BINARY_DIR};${LIBAEC_SOURCE_DIR}/src;${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
)

if (MSVC)
  set (CMAKE_MFC_FLAG 0)
  set(CMAKE_DEBUG_POSTFIX "d")
endif ()

set (EXE_EXT "")
if (WIN32)
  set (EXE_EXT ".exe")
  add_compile_definitions (_BIND_TO_CURRENT_VCLIBS_VERSION=1)
  add_compile_definitions (_CRT_SECURE_NO_WARNINGS)
  add_compile_definitions (_CONSOLE)
endif ()

#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
   set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmessage-length=0")
endif ()

#-----------------------------------------------------------------------------
# Generate the aec_config.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
configure_file (${LIBAEC_RESOURCES_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
# Create libaec.h with current version information
configure_file(
  "include/libaec.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/libaec.h")

#-----------------------------------------------------------------------------
# Define LIBAEC Library
#-----------------------------------------------------------------------------

set (LIBAEC_PUBLIC_HEADERS
    ${CMAKE_CURRENT_BINARY_DIR}/libaec.h
)

set(LIBAEC_SRCS
    ${LIBAEC_SRC_DIR}/encode.c
    ${LIBAEC_SRC_DIR}/encode_accessors.c
    ${LIBAEC_SRC_DIR}/decode.c
    ${LIBAEC_SRC_DIR}/vector.c
)

add_library (${LIBAEC_LIB_TARGET} STATIC ${LIBAEC_SRCS} ${LIBAEC_PUBLIC_HEADERS})
target_include_directories (${LIBAEC_LIB_TARGET} PUBLIC
  "$<BUILD_INTERFACE:${LIBAEC_SOURCE_DIR}>"
  "$<BUILD_INTERFACE:${LIBAEC_SOURCE_DIR}/include>"
  "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>"
  "$<INSTALL_INTERFACE:include>")
TARGET_C_PROPERTIES (${LIBAEC_LIB_TARGET} STATIC)
H5_SET_LIB_OPTIONS (${LIBAEC_LIB_TARGET} ${LIBAEC_LIB_NAME} STATIC 0)
set_target_properties (${LIBAEC_LIB_TARGET} PROPERTIES
    VERSION 0.0.12 SOVERSION 0
    PUBLIC_HEADER ""
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set_global_variable (LIBAEC_LIBRARIES_TO_EXPORT ${LIBAEC_LIB_TARGET})
set (install_targets ${LIBAEC_LIB_TARGET})

set(SZIP_SRCS
    ${LIBAEC_SRC_DIR}/sz_compat.c
)

set (SZIP_PUBLIC_HEADERS
    ${LIBAEC_INC_DIR}/szlib.h
)

add_library (${SZIP_LIB_TARGET} STATIC ${SZIP_SRCS} ${SZIP_PUBLIC_HEADERS})
target_include_directories (${SZIP_LIB_TARGET} PUBLIC "${LIBAEC_SOURCE_DIR};${LIBAEC_SOURCE_DIR}/include;${CMAKE_BINARY_DIR}")
TARGET_C_PROPERTIES (${SZIP_LIB_TARGET} STATIC)
target_link_libraries (${SZIP_LIB_TARGET} PRIVATE ${LIBAEC_LIB_TARGET})
H5_SET_LIB_OPTIONS (${SZIP_LIB_TARGET} ${SZIP_LIB_NAME} STATIC 0)
set_target_properties (${SZIP_LIB_TARGET} PROPERTIES
    VERSION 2.0.1 SOVERSION 2
    PUBLIC_HEADER ""
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

set_global_variable (LIBAEC_LIBRARIES_TO_EXPORT "${LIBAEC_LIBRARIES_TO_EXPORT};${SZIP_LIB_TARGET}")
set (install_targets ${install_targets} ${SZIP_LIB_TARGET})

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (LIBAEC_EXPORTED_TARGETS)
  INSTALL_TARGET_PDB (${LIBAEC_LIB_TARGET} ${LIBAEC_INSTALL_BIN_DIR} libraries)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${LIBAEC_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${LIBAEC_INSTALL_LIB_DIR} COMPONENT libraries
      ARCHIVE DESTINATION ${LIBAEC_INSTALL_LIB_DIR} COMPONENT libraries
      RUNTIME DESTINATION ${LIBAEC_INSTALL_BIN_DIR} COMPONENT libraries
      FRAMEWORK DESTINATION ${LIBAEC_INSTALL_FWRK_DIR} COMPONENT libraries
      PUBLIC_HEADER DESTINATION ${LIBAEC_INSTALL_INCLUDE_DIR} COMPONENT headers
  )
endif ()

include (CMakePackageConfigHelpers)

#-----------------------------------------------------------------------------
# Set includes needed for build
#-----------------------------------------------------------------------------
set (LIBAEC_INCLUDES_BUILD_TIME
    ${LIBAEC_SRC_DIR} ${LIBAEC_INC_DIR} ${LIBAEC_BINARY_DIR}
)

#-----------------------------------------------------------------------------
# Set variables needed for installation
#-----------------------------------------------------------------------------
set (LIBAEC_VERSION_STRING ${LIBAEC_PACKAGE_VERSION})
set (LIBAEC_VERSION_MAJOR  ${LIBAEC_PACKAGE_VERSION_MAJOR})
set (LIBAEC_VERSION_MINOR  ${LIBAEC_PACKAGE_VERSION_MINOR})

#-----------------------------------------------------------------------------
# Configure the libaec-config.cmake file for the build directory
#-----------------------------------------------------------------------------
set (INCLUDE_INSTALL_DIR ${LIBAEC_INSTALL_INCLUDE_DIR})
set (SHARE_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/${LIBAEC_INSTALL_CMAKE_DIR}" )
set (CURRENT_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}" )
configure_package_config_file (
    ${LIBAEC_RESOURCES_DIR}/libaec-config.cmake.in
    "${LIBAEC_BINARY_DIR}/${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-config.cmake"
    INSTALL_DESTINATION "${LIBAEC_INSTALL_CMAKE_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR SHARE_INSTALL_DIR CURRENT_BUILD_DIR
    INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
)

#-----------------------------------------------------------------------------
# Configure the libaec-config.cmake file for the install directory
#-----------------------------------------------------------------------------
set (INCLUDE_INSTALL_DIR ${LIBAEC_INSTALL_INCLUDE_DIR})
set (SHARE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${LIBAEC_INSTALL_CMAKE_DIR}" )
set (CURRENT_BUILD_DIR "${CMAKE_INSTALL_PREFIX}")
configure_package_config_file (
    ${LIBAEC_RESOURCES_DIR}/libaec-config.cmake.in
    "${LIBAEC_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-config.cmake"
    INSTALL_DESTINATION "${LIBAEC_INSTALL_CMAKE_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR SHARE_INSTALL_DIR CURRENT_BUILD_DIR
)
if (NOT LIBAEC_EXTERNALLY_CONFIGURED)
  install (
      FILES ${LIBAEC_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-config.cmake
      DESTINATION ${LIBAEC_INSTALL_CMAKE_DIR}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Configure the libaec-config-version.cmake file for the install directory
#-----------------------------------------------------------------------------
if (NOT LIBAEC_EXTERNALLY_CONFIGURED)
  configure_file (
      ${LIBAEC_RESOURCES_DIR}/libaec-config-version.cmake.in
      ${LIBAEC_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-config-version.cmake @ONLY
  )
  install (
      FILES ${LIBAEC_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-config-version.cmake
      DESTINATION ${LIBAEC_INSTALL_CMAKE_DIR}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (NOT LIBAEC_EXTERNALLY_CONFIGURED)
  install (
      EXPORT ${LIBAEC_EXPORTED_TARGETS}
      DESTINATION ${LIBAEC_INSTALL_CMAKE_DIR}
      FILE ${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-targets.cmake
      NAMESPACE ${PACKAGE_NAMESPACE}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Export all exported targets to the build tree for use by parent project
#-----------------------------------------------------------------------------
if (NOT LIBAEC_EXTERNALLY_CONFIGURED)
  export (
      TARGETS ${LIBAEC_LIBRARIES_TO_EXPORT} ${LIBAEC_LIB_DEPENDENCIES}
      FILE ${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT}-targets.cmake
      NAMESPACE ${PACKAGE_NAMESPACE}
  )
  export (PACKAGE ${LIBAEC_PACKAGE}${LIBAEC_PACKAGE_EXT})
endif ()

