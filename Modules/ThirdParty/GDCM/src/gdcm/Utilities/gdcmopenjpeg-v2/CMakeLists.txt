# Main CMakeLists.txt to build the OpenJPEG project using CMake (www.cmake.org)
# Written by Mathieu Malaterre

# This CMake project will by default create a library called openjpeg
# But if you want to use this project within your own (CMake) project
# you will eventually like to prefix the library to avoid linking confusion
# For this purpose you can define a CMake var: OPENJPEG_NAMESPACE to whatever you like
# e.g.:
# set(OPENJPEG_NAMESPACE "GDCMOPENJPEG")
cmake_minimum_required(VERSION 2.8.9)

if(NOT OPENJPEG_NAMESPACE)
  set(OPENJPEG_NAMESPACE "OPENJPEG")
  set(OPENJPEG_STANDALONE 1)
endif()
# In all cases:
string(TOLOWER ${OPENJPEG_NAMESPACE} OPENJPEG_LIBRARY_NAME)

project(${OPENJPEG_NAMESPACE} C)

# Do full dependency headers.
include_regular_expression("^.*$")

#-----------------------------------------------------------------------------
# OPENJPEG version number, useful for packaging and doxygen doc:
set(OPENJPEG_VERSION_MAJOR 2)
set(OPENJPEG_VERSION_MINOR 0)
set(OPENJPEG_VERSION_BUILD 0)
set(OPENJPEG_VERSION
  "${OPENJPEG_VERSION_MAJOR}.${OPENJPEG_VERSION_MINOR}.${OPENJPEG_VERSION_BUILD}")

# This setting of SOVERSION assumes that any API change
# will increment either the minor or major version number of openjpeg
set(OPENJPEG_LIBRARY_PROPERTIES
  VERSION   "${OPENJPEG_VERSION_MAJOR}.${OPENJPEG_VERSION_MINOR}.${OPENJPEG_VERSION_BUILD}"
  SOVERSION "${OPENJPEG_VERSION_MAJOR}.${OPENJPEG_VERSION_MINOR}"
)
# You will also need to define a value for the following variables:
# OPENJPEG_INSTALL_BIN_DIR          - binary dir (executables)
# OPENJPEG_INSTALL_LIB_DIR          - library dir (libs)
# OPENJPEG_INSTALL_DATA_DIR         - share dir (say, examples, data, etc)
# OPENJPEG_INSTALL_INCLUDE_DIR      - include dir (headers)

# --------------------------------------------------------------------------
# Install directories

string(TOLOWER ${PROJECT_NAME} projectname)
set(subdir "${projectname}-${OPENJPEG_VERSION_MAJOR}.${OPENJPEG_VERSION_MINOR}")

if(NOT OPENJPEG_INSTALL_BIN_DIR)
  set(OPENJPEG_INSTALL_BIN_DIR "bin")
endif()

if(NOT OPENJPEG_INSTALL_LIB_DIR)
  set(OPENJPEG_INSTALL_LIB_DIR "lib")
endif()

if(NOT OPENJPEG_INSTALL_DATA_DIR)
  set(OPENJPEG_INSTALL_DATA_DIR "share/${subdir}")
endif()

if(NOT OPENJPEG_INSTALL_INCLUDE_DIR)
  set(OPENJPEG_INSTALL_INCLUDE_DIR "include/${subdir}")
endif()

if(NOT OPENJPEG_INSTALL_DOC_DIR)
  set(OPENJPEG_INSTALL_DOC_DIR "share/doc/${subdir}")
endif()

if(NOT OPENJPEG_INSTALL_PACKAGE_DIR)
  set(OPENJPEG_INSTALL_PACKAGE_DIR ${OPENJPEG_INSTALL_LIB_DIR}/${subdir}
    CACHE INTERNAL "")
endif()

#-----------------------------------------------------------------------------
# Test for some required system information.
include (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityC.cmake)

# configure name mangling to allow multiple libraries to coexist
# peacefully
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/openjpeg_mangle.h.in)
set(MANGLE_PREFIX ${OPENJPEG_LIBRARY_NAME})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/openjpeg_mangle.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/openjpeg_mangle.h
               @ONLY)
endif()

#-----------------------------------------------------------------------------
# Configure files with settings for use by the build.
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/opj_configure.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/opj_configure.h)


#-----------------------------------------------------------------------------
# Always build the library
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})
subdirs(
  libopenjpeg
  )
