cmake_minimum_required (VERSION 3.12)
PROJECT (ZLIB C)

#-----------------------------------------------------------------------------
# Basic ZLIB stuff here
#-----------------------------------------------------------------------------
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
#
# Check for unistd.h
#
check_include_file(unistd.h Z_HAVE_UNISTD_H)
CHECK_FUNCTION_EXISTS (memcpy            HAVE_MEMCPY)
CHECK_FUNCTION_EXISTS (vsnprintf         HAVE_VSNPRINTF)

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (ZLIB_RESOURCES_DIR          ${HDF_RESOURCES_DIR}/ZLIB)
set (ZLIB_SRC_DIR                ${ZLIB_SOURCE_DIR})

#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
set (ZLIB_LIB_CORENAME           "zlib")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
set (ZLIB_LIB_NAME              "${ZLIB_EXTERNAL_LIB_PREFIX}${ZLIB_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
set (ZLIB_LIB_TARGET             "${ZLIB_LIB_CORENAME}-static")

set(ZLIB_PC ${CMAKE_CURRENT_BINARY_DIR}/zlib.pc)
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/zlib.pc.cmakein ${ZLIB_PC} @ONLY)

#-----------------------------------------------------------------------------
# Generate the zconf.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/zconf.h.cmakein ${CMAKE_CURRENT_BINARY_DIR}/zconf.h @ONLY)

if (DEFINED ADDITIONAL_CMAKE_PREFIX_PATH AND EXISTS "${ADDITIONAL_CMAKE_PREFIX_PATH}")
  set (CMAKE_PREFIX_PATH ${ADDITIONAL_CMAKE_PREFIX_PATH} ${CMAKE_PREFIX_PATH})
endif ()

#-----------------------------------------------------------------------------
# parse the full version number from zlib.h and include in ZLIB_VERS_INFO
#-----------------------------------------------------------------------------
file (READ ${ZLIB_SRC_DIR}/zlib.h _zlib_h_contents)
string (REGEX REPLACE ".*#define[ \t]+ZLIB_VER_MAJOR[ \t]+([0-9]*).*$"
    "\\1" ZLIB_VERS_MAJOR ${_zlib_h_contents})
string (REGEX REPLACE ".*#define[ \t]+ZLIB_VER_MINOR[ \t]+([0-9]*).*$"
    "\\1" ZLIB_VERS_MINOR ${_zlib_h_contents})
string (REGEX REPLACE ".*#define[ \t]+ZLIB_VER_REVISION[ \t]+([0-9]*).*$"
    "\\1" ZLIB_VERS_RELEASE ${_zlib_h_contents})
string (REGEX REPLACE ".*#define[ \t]+ZLIB_VER_SUBREVISION[ \t]+([0-9]*).*$"
    "\\1" ZLIB_VERS_SUBRELEASE ${_zlib_h_contents})
#message (STATUS "VERSION: ${ZLIB_VERS_MAJOR}.${ZLIB_VERS_MINOR}.${ZLIB_VERS_RELEASE}-${ZLIB_VERS_SUBRELEASE}")
string (REGEX REPLACE ".*#define[ \t]+ZLIB_VERSION[ \t]+\"([0-9A-Za-z.]+)\".*"
    "\\1" ZLIB_FULL_VERSION ${_zlib_h_contents})
#message (STATUS "VERSION: ${ZLIB_FULL_VERSION}")

#-----------------------------------------------------------------------------
set (ZLIB_PACKAGE "zlib")
set (ZLIB_PACKAGE_NAME "ZLIB")
set (ZLIB_PACKAGE_VERSION "${ZLIB_VERS_MAJOR}.${ZLIB_VERS_MINOR}")
set (ZLIB_PACKAGE_VERSION_MAJOR "${ZLIB_VERS_MAJOR}.${ZLIB_VERS_MINOR}")
set (ZLIB_PACKAGE_VERSION_MINOR "${ZLIB_VERS_RELEASE}")
set (ZLIB_PACKAGE_STRING "${ZLIB_PACKAGE_NAME} ${ZLIB_PACKAGE_VERSION}-${ZLIB_VERS_SUBRELEASE}")
set (ZLIB_PACKAGE_TARNAME "zlib")
set (ZLIB_PACKAGE_URL "http://www.hdfgroup.org")
set (ZLIB_PACKAGE_BUGREPORT "help@hdfgroup.org")
set (ZLIB_PACKAGE_SOVERSION "${ZLIB_VERS_MAJOR}.${ZLIB_VERS_MINOR}.${ZLIB_VERS_RELEASE}")
set (ZLIB_PACKAGE_SOVERSION_MAJOR "${ZLIB_VERS_MAJOR}")


HDF_DIR_PATHS(${ZLIB_PACKAGE_NAME})

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXPORTED_TARGETS)
  set (ZLIB_EXPORTED_TARGETS "zlib-targets")
endif ()

#-----------------------------------------------------------------------------
# To include a library in the list exported by the project AT BUILD TIME,
# add it to this variable. This is NOT used by Make Install, but for projects
# which include zlib as a sub-project within their build tree
#-----------------------------------------------------------------------------
set_global_variable (ZLIB_LIBRARIES_TO_EXPORT "")

set (CMAKE_POSITION_INDEPENDENT_CODE ON)

#-----------------------------------------------------------------------------
# When building utility executables that generate other (source) files :
# we make use of the following variables defined in the root CMakeLists.
# Certain systems may add /Debug or /Release to output paths
# and we need to call the executable from inside the CMake configuration
#-----------------------------------------------------------------------------
set (EXE_EXT "")
if (WIN32)
  set (EXE_EXT ".exe")
  add_definitions (-D_BIND_TO_CURRENT_VCLIBS_VERSION=1)
  add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  add_definitions (-D_CONSOLE)
  add_definitions (-D_CRT_NONSTDC_NO_DEPRECATE)
endif ()

if (MSVC)
  set (CMAKE_MFC_FLAG 0)
endif ()

#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -Wno-strict-prototypes")
endif ()
if (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
   set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -Wno-deprecated-non-prototype -Wno-implicit-function-declaration")
endif ()

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmessage-length=0")
endif ()

#-----------------------------------------------------------------------------
# All libs/tests/examples need the main include directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${ZLIB_BINARY_DIR} ${ZLIB_SOURCE_DIR} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

#============================================================================
# zlib
#============================================================================

#-----------------------------------------------------------------------------
# Define zlib Library
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

if(NOT MINGW)
    set(ZLIB_DLL_SRCS
        win32/zlib1.rc # If present will override custom build rule below.
    )
endif()

if(CMAKE_COMPILER_IS_GNUCC)
    if(ASM686)
        set(ZLIB_ASMS contrib/asm686/match.S)
    elseif (AMD64)
        set(ZLIB_ASMS contrib/amd64/amd64-match.S)
    endif ()

    if(ZLIB_ASMS)
        add_definitions(-DASMV)
        set_source_files_properties (${ZLIB_ASMS} PROPERTIES LANGUAGE C COMPILE_FLAGS -DNO_UNDERLINE)
    endif()
endif()

if(MSVC)
    if(ASM686)
        enable_language(ASM_MASM)
        set(ZLIB_ASMS
            contrib/masmx86/inffas32.asm
            contrib/masmx86/match686.asm
        )
    elseif (AMD64)
        enable_language(ASM_MASM)
        set(ZLIB_ASMS
            contrib/masmx64/gvmat64.asm
            contrib/masmx64/inffasx64.asm
        )
    endif()

    if(ZLIB_ASMS)
        add_definitions(-DASMV -DASMINF)
    endif()
endif()

if(MINGW)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/zlib1rc.obj
                       COMMAND ${CMAKE_RC_COMPILER}
                            -D GCC_WINDRES
                            -I ${CMAKE_CURRENT_SOURCE_DIR}
                            -I ${CMAKE_CURRENT_BINARY_DIR}
                            -o ${CMAKE_CURRENT_BINARY_DIR}/zlib1rc.obj
                            -i ${CMAKE_CURRENT_SOURCE_DIR}/win32/zlib1.rc)
    set(ZLIB_DLL_SRCS ${CMAKE_CURRENT_BINARY_DIR}/zlib1rc.obj)
endif()

add_library (${ZLIB_LIB_TARGET} STATIC ${ZLIB_SRCS} ${ZLIB_PRIVATE_HDRS} ${ZLIB_PUBLIC_HDRS})
if (MSVC AND CMAKE_CL_64)
  set_target_properties (${ZLIB_LIB_TARGET} PROPERTIES STATIC_LIBRARY_FLAGS "/machine:x64")
endif ()
target_include_directories(${ZLIB_LIB_TARGET} PRIVATE "${CMAKE_BINARY_DIR}")
TARGET_C_PROPERTIES (${ZLIB_LIB_TARGET} STATIC)
target_link_libraries (${ZLIB_LIB_TARGET} PRIVATE ${LINK_LIBS})
H5_SET_LIB_OPTIONS (${ZLIB_LIB_TARGET} ${ZLIB_LIB_NAME} STATIC 0)
set_target_properties(${ZLIB_LIB_TARGET} PROPERTIES
    PUBLIC_HEADER "${ZLIB_PUBLIC_HEADERS}"
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set_global_variable (ZLIB_LIBRARIES_TO_EXPORT ${ZLIB_LIB_TARGET})
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
# Check for Installation Utilities
#-----------------------------------------------------------------------------
if (WIN32)
  set (PF_ENV_EXT "(x86)")
  find_program (NSIS_EXECUTABLE NSIS.exe PATHS "$ENV{ProgramFiles}\\NSIS" "$ENV{ProgramFiles${PF_ENV_EXT}}\\NSIS")
  if(NOT CPACK_WIX_ROOT)
    file(TO_CMAKE_PATH "$ENV{WIX}" CPACK_WIX_ROOT)
  endif ()
  find_program (WIX_EXECUTABLE candle  PATHS "${CPACK_WIX_ROOT}/bin")
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
if (NOT ZLIB_INSTALL_NO_DEVELOPMENT)
  install (
      FILES ${PROJECT_BINARY_DIR}/zconf.h
      DESTINATION ${ZLIB_INSTALL_INCLUDE_DIR}
      COMPONENT headers
  )
endif ()

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  install (
      EXPORT ${ZLIB_EXPORTED_TARGETS}
      DESTINATION ${ZLIB_INSTALL_CMAKE_DIR}
      FILE ${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-targets.cmake
      NAMESPACE ${PACKAGE_NAMESPACE}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Export all exported targets to the build tree for use by parent project
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  export (
      TARGETS ${ZLIB_LIBRARIES_TO_EXPORT} ${ZLIB_LIB_DEPENDENCIES}
      FILE ${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-targets.cmake
      NAMESPACE ${PACKAGE_NAMESPACE}
  )
  export (PACKAGE ${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT})
endif ()

#-----------------------------------------------------------------------------
# Set includes needed for build
#-----------------------------------------------------------------------------
set (ZLIB_INCLUDES_BUILD_TIME
    ${ZLIB_SRC_DIR} ${ZLIB_BINARY_DIR}
)

#-----------------------------------------------------------------------------
# Set variables needed for installation
#-----------------------------------------------------------------------------
set (ZLIB_VERSION_STRING ${ZLIB_PACKAGE_VERSION})
set (ZLIB_VERSION_MAJOR  ${ZLIB_PACKAGE_VERSION_MAJOR})
set (ZLIB_VERSION_MINOR  ${ZLIB_PACKAGE_VERSION_MINOR})

#-----------------------------------------------------------------------------
# Configure the zlib-config.cmake file for the build directory
#-----------------------------------------------------------------------------
set (INCLUDE_INSTALL_DIR ${ZLIB_INSTALL_INCLUDE_DIR})
set (SHARE_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/${ZLIB_INSTALL_CMAKE_DIR}" )
set (CURRENT_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}" )
configure_package_config_file (
    ${ZLIB_RESOURCES_DIR}/zlib-config.cmake.in
    "${ZLIB_BINARY_DIR}/${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-config.cmake"
    INSTALL_DESTINATION "${ZLIB_INSTALL_CMAKE_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR SHARE_INSTALL_DIR CURRENT_BUILD_DIR
    INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
)

#-----------------------------------------------------------------------------
# Configure the zlib-config.cmake file for the install directory
#-----------------------------------------------------------------------------
set (INCLUDE_INSTALL_DIR ${ZLIB_INSTALL_INCLUDE_DIR})
set (SHARE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${ZLIB_INSTALL_CMAKE_DIR}" )
set (CURRENT_BUILD_DIR "${CMAKE_INSTALL_PREFIX}" )
configure_package_config_file (
    ${ZLIB_RESOURCES_DIR}/zlib-config.cmake.in
    "${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-config.cmake"
    INSTALL_DESTINATION "${ZLIB_INSTALL_CMAKE_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR SHARE_INSTALL_DIR CURRENT_BUILD_DIR
)
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  install (
      FILES ${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-config.cmake
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
      ${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-config-version.cmake @ONLY
  )
  install (
      FILES ${ZLIB_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${ZLIB_PACKAGE}${ZLIB_PACKAGE_EXT}-config-version.cmake
      DESTINATION ${ZLIB_INSTALL_CMAKE_DIR}
      COMPONENT configinstall
  )
endif ()

#-----------------------------------------------------------------------------
# Add Document File(s) to CMake Install
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  install (
      FILES
          ${ZLIB_SOURCE_DIR}/FAQ
          ${ZLIB_SOURCE_DIR}/README
          ${ZLIB_SOURCE_DIR}/INDEX
      DESTINATION ${ZLIB_INSTALL_DATA_DIR}
      COMPONENT documents
  )
endif ()

#-----------------------------------------------------------------------------
# Check for Installation Utilities
#-----------------------------------------------------------------------------
if (WIN32)
  set (PF_ENV_EXT "(x86)")
  find_program (NSIS_EXECUTABLE NSIS.exe PATHS "$ENV{ProgramFiles}\\NSIS" "$ENV{ProgramFiles${PF_ENV_EXT}}\\NSIS")
  if(NOT CPACK_WIX_ROOT)
    file(TO_CMAKE_PATH "$ENV{WIX}" CPACK_WIX_ROOT)
  endif()
  find_program (WIX_EXECUTABLE candle  PATHS "${CPACK_WIX_ROOT}/bin")
endif ()

#-----------------------------------------------------------------------------
# Configure the LICENSE.txt file for the windows binary package
#-----------------------------------------------------------------------------
if (WIN32)
  configure_file (${ZLIB_SOURCE_DIR}/LICENSE ${ZLIB_BINARY_DIR}/ZLIB_LICENSE.txt @ONLY)
else ()
  configure_file (${ZLIB_SOURCE_DIR}/LICENSE ${ZLIB_BINARY_DIR}/ZLIB_LICENSE @ONLY)
endif ()
configure_file (${ZLIB_SOURCE_DIR}/README ${ZLIB_BINARY_DIR}/ZLIB_README @ONLY)

#-----------------------------------------------------------------------------
# Set the cpack variables
#-----------------------------------------------------------------------------
if (NOT ZLIB_EXTERNALLY_CONFIGURED)
  set (CPACK_PACKAGE_VENDOR "HDF_Group")
  set (CPACK_PACKAGE_NAME "${ZLIB_PACKAGE_NAME}")
  if (CDASH_LOCAL)
    set (CPACK_PACKAGE_VERSION "${ZLIB_PACKAGE_VERSION}")
  else ()
    set (CPACK_PACKAGE_VERSION "${ZLIB_PACKAGE_VERSION_STRING}")
  endif ()
  set (CPACK_PACKAGE_VERSION_MAJOR "${ZLIB_PACKAGE_VERSION_MAJOR}")
  set (CPACK_PACKAGE_VERSION_MINOR "${ZLIB_PACKAGE_VERSION_MINOR}")
  set (CPACK_PACKAGE_VERSION_PATCH "")
  set (CPACK_RESOURCE_FILE_LICENSE "${ZLIB_BINARY_DIR}/ZLIB_LICENSE")
  set (CPACK_PACKAGE_DESCRIPTION_FILE "${ZLIB_BINARY_DIR}/ZLIB_README")
  set (CPACK_RESOURCE_FILE_README "${ZLIB_BINARY_DIR}/ZLIB_README")
  set (CPACK_PACKAGE_RELOCATABLE TRUE)
  set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "zlib Installation")
  set (CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_VENDOR}/${CPACK_PACKAGE_NAME}/${CPACK_PACKAGE_VERSION}")

  set (CPACK_GENERATOR "TGZ")
  if (WIN32)
    set (CPACK_GENERATOR "ZIP")

    if (NSIS_EXECUTABLE)
      list (APPEND CPACK_GENERATOR "NSIS")
    endif ()
    # Installers for 32- vs. 64-bit CMake:
    #  - Root install directory (displayed to end user at installer-run time)
    #  - "NSIS package/display name" (text used in the installer GUI)
    #  - Registry key used to store info about the installation
    set (CPACK_NSIS_PACKAGE_NAME "${ZLIB_PACKAGE_STRING}")
    if (CMAKE_CL_64)
      set (CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
      set (CPACK_PACKAGE_INSTALL_REGISTRY_KEY "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION} (Win64)")
    else ()
      set (CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
      set (CPACK_PACKAGE_INSTALL_REGISTRY_KEY "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}")
    endif ()
    set (CPACK_NSIS_DISPLAY_NAME "${CPACK_NSIS_PACKAGE_NAME}")
    set (CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_VENDOR}\\\\${CPACK_PACKAGE_NAME}\\\\${CPACK_PACKAGE_VERSION}")
    set (CPACK_NSIS_CONTACT "${ZLIB_PACKAGE_BUGREPORT}")
    set (CPACK_NSIS_MODIFY_PATH ON)
    set (CPACK_NSIS_PACKAGE_NAME "ZLIB ${ZLIB_PACKAGE_VERSION}")
    if (WIX_EXECUTABLE)
      list (APPEND CPACK_GENERATOR "WIX")
    endif ()
#WiX variables
    set (CPACK_WIX_UNINSTALL "1")
    set (CPACK_RESOURCE_FILE_LICENSE "${ZLIB_BINARY_DIR}/ZLIB_LICENSE.txt")
  elseif (APPLE)
    list (APPEND CPACK_GENERATOR "STGZ")
    list (APPEND CPACK_GENERATOR "DragNDrop")
    set (CPACK_COMPONENTS_ALL_IN_ONE_PACKAGE ON)
    set (CPACK_PACKAGING_INSTALL_PREFIX "/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
    #set (CPACK_PACKAGE_ICON "${ZLIB_RESOURCES_DIR}/hdf.icns")

    option (ZLIB_PACK_MACOSX_FRAMEWORK  "Package the ZLIB Library in a Framework" OFF)
    if (ZLIB_PACK_MACOSX_FRAMEWORK AND ZLIB_BUILD_FRAMEWORKS)
      set (CPACK_BUNDLE_NAME "${ZLIB_PACKAGE_STRING}")
      set (CPACK_BUNDLE_LOCATION "/")    # make sure CMAKE_INSTALL_PREFIX ends in /
      set (CMAKE_INSTALL_PREFIX "/${CPACK_BUNDLE_NAME}.framework/Versions/${CPACK_PACKAGE_VERSION}/${CPACK_PACKAGE_NAME}/")
      set (CPACK_SHORT_VERSION_STRING "${CPACK_PACKAGE_VERSION}")
      #-----------------------------------------------------------------------------
      # Configure the Info.plist file for the install bundle
      #-----------------------------------------------------------------------------
      configure_file (
          ${ZLIB_RESOURCES_DIR}/CPack.Info.plist.in
          ${ZLIB_BINARY_DIR}/CMakeFiles/Info.plist @ONLY
      )
      configure_file (
          ${ZLIB_RESOURCES_DIR}/PkgInfo.in
          ${ZLIB_BINARY_DIR}/CMakeFiles/PkgInfo @ONLY
      )
      install (
          FILES ${ZLIB_BINARY_DIR}/CMakeFiles/PkgInfo
          DESTINATION ..
      )
    endif ()
  else ()
    list (APPEND CPACK_GENERATOR "STGZ")
    set (CPACK_PACKAGING_INSTALL_PREFIX "/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
    set (CPACK_COMPONENTS_ALL_IN_ONE_PACKAGE ON)

    set (CPACK_DEBIAN_PACKAGE_SECTION "Libraries")
    set (CPACK_DEBIAN_PACKAGE_MAINTAINER "${ZLIB_PACKAGE_BUGREPORT}")

#      list (APPEND CPACK_GENERATOR "RPM")
    set (CPACK_RPM_PACKAGE_RELEASE "1")
    set (CPACK_RPM_COMPONENT_INSTALL ON)
    set (CPACK_RPM_PACKAGE_RELOCATABLE ON)
  endif ()

  # By default, do not warn when built on machines using only VS Express:
  if (NOT DEFINED CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS)
    set (CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  endif ()
  include (InstallRequiredSystemLibraries)

  set (CPACK_INSTALL_CMAKE_PROJECTS "${ZLIB_BINARY_DIR};ZLIB;libraries;/")
  set (CPACK_INSTALL_CMAKE_PROJECTS "${ZLIB_BINARY_DIR};ZLIB;headers;/")
  set (CPACK_INSTALL_CMAKE_PROJECTS "${ZLIB_BINARY_DIR};ZLIB;configinstall;/")

  set (CPACK_ALL_INSTALL_TYPES Full Developer User)
  set (CPACK_INSTALL_TYPE_FULL_DISPLAY_NAME "Everything")

  set(CPACK_COMPONENTS_ALL libraries headers documents configinstall)

  include (CPack)

  cpack_add_component_group(Runtime)

  cpack_add_component_group(Documents
      EXPANDED
      DESCRIPTION "Release notes for zlib"
  )

  cpack_add_component_group(Development
      EXPANDED
      DESCRIPTION "All of the tools you'll need to develop applications"
  )

  cpack_add_component (libraries
      DISPLAY_NAME "ZLIB Libraries"
      REQUIRED
      GROUP Runtime
      INSTALL_TYPES Full Developer User
  )
  cpack_add_component (headers
      DISPLAY_NAME "ZLIB Headers"
      DEPENDS libraries
      GROUP Development
      INSTALL_TYPES Full Developer
  )
  cpack_add_component (documents
      DISPLAY_NAME "ZLIB Documents"
      GROUP Documents
      INSTALL_TYPES Full Developer
  )
  cpack_add_component (configinstall
      DISPLAY_NAME "ZLIB CMake files"
      DEPENDS libraries
      GROUP Development
      INSTALL_TYPES Full Developer User
  )

endif ()
