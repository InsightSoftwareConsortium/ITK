#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

cmake_minimum_required(VERSION 2.8.6 FATAL_ERROR)
if(POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW)
endif()
if(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif()

project(WrapITK)

# set an internal cache to mark when this file is parsed
set(ITK_WRAPPING_REACHED ON CACHE INTERNAL "Wrapping folder has been reached")

include("CMakeUtilityFunctions.cmake")

set(ITK_VERSION "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}")
if(NOT "${CMAKE_VERSION}" STRLESS "2.6.3")
  cmake_policy(SET CMP0011 OLD)
endif()

enable_testing()

###############################################################################
# Config ITKWrap
###############################################################################

# Set up options needed for ConfigureWrapping

option(ITK_WRAP_unsigned_char "Wrap unsigned char type" ON)
option(ITK_WRAP_unsigned_short "Wrap unsigned short type" OFF)
option(ITK_WRAP_unsigned_long "Wrap unsigned long type" OFF)

option(ITK_WRAP_signed_char "Wrap signed char type" OFF)
option(ITK_WRAP_signed_short "Wrap signed short type" ON)
option(ITK_WRAP_signed_long "Wrap signed long type" OFF)

option(ITK_WRAP_float "Wrap float type" ON)
option(ITK_WRAP_double "Wrap double type" OFF)

option(ITK_WRAP_vector_float "Wrap vector float type" ON)
option(ITK_WRAP_vector_double "Wrap vector double type" OFF)

option(ITK_WRAP_covariant_vector_float "Wrap covariant vector float type" ON)
option(ITK_WRAP_covariant_vector_double "Wrap covariant vector double type" OFF)

option(ITK_WRAP_rgb_unsigned_char "Wrap RGB< unsigned char > type" ON)
option(ITK_WRAP_rgb_unsigned_short "Wrap RGB< unsigned short > type" OFF)

option(ITK_WRAP_rgba_unsigned_char "Wrap RGBA< unsigned char > type" ON)
option(ITK_WRAP_rgba_unsigned_short "Wrap RGBA< unsigned short > type" OFF)

option(ITK_WRAP_complex_float "Wrap complex<float> type" ON)
option(ITK_WRAP_complex_double "Wrap complex<double> type" OFF)

set(ITK_WRAP_DIMS "2;3" CACHE STRING "Dimensions available separated by semicolons (;)")

option(INSTALL_WRAP_ITK_COMPATIBILITY "Install FindWrapITK.cmake and WrapITK.pth (so cmake and python can find WrapITK) as part of the standard installation process. This requires admin privileges. These files can be separately installed by making the install_wrapitk_compatibility target. " ON)

if(ITK_INSTALL_PACKAGE_DIR)
  string(REGEX REPLACE "^/" "" path "${ITK_INSTALL_PACKAGE_DIR}/WrapITK/")
else()
  set(path "lib/InsightToolkit/WrapITK/")
endif()
set(WRAP_ITK_INSTALL_PREFIX "${path}" CACHE INTERNAL "subpath where where most of WrapITK files will be installed")

# Output directories.
set(WRAP_ITK_CONFIG_DIR "${WrapITK_SOURCE_DIR}/")
set(WRAP_ITK_CMAKE_DIR "${WrapITK_SOURCE_DIR}")
set(ITK_WRAP_JAVA_DIR "${WrapITK_BINARY_DIR}/Java")

if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${WrapITK_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
endif()

if(NOT EXECUTABLE_OUTPUT_PATH)
  set(EXECUTABLE_OUTPUT_PATH ${WrapITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
endif()

mark_as_advanced(CMAKE_LIBRARY_OUTPUT_DIRECTORY EXECUTABLE_OUTPUT_PATH WRAP_ITK_INSTALL_PREFIX)

set(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

###############################################################################
# Additional files for installation
###############################################################################

install(FILES
  CMakeUtilityFunctions.cmake
  ConfigureWrapping.cmake
  TypedefMacros.cmake
  WrapBasicTypes.cmake
  WrapITKTypes.cmake
  DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration"
)

install(FILES "${WrapITK_BINARY_DIR}/InstallOnly/WrapITKConfig.cmake"
  DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/"
)

# Add a custom target to drive installation of the WrapITK-compatibility files
if(INSTALL_WRAP_ITK_COMPATIBILITY)
  # if we should by default install the files, make a custom target that will
  # always be built.
  CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET(install_wrapitk_compatibility DEFAULT
    "Installing WrapITK compatibility files. Requires admin priviliges: turn off INSTALL_WRAP_ITK_COMPATIBILITY in cmake to disable.")
else()
  # otherwise, add a target that will install the files only when that target is
  # manually built.
  CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET(install_wrapitk_compatibility ON_DEMAND
    "Installing WrapITK compatibility files. Requires admin priviliges.")
endif()

INSTALL_AT_ABSOLUTE_PATH(install_wrapitk_compatibility "${CMAKE_ROOT}/Modules" "${WrapITK_BINARY_DIR}/InstallOnly/FindWrapITK.cmake")

set(WRAP_ITK_TYPEDEFS_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/Typedefs")
set(WRAP_ITK_LIB_DIRECTORY  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")

###############################################################################
# Setup test driver
###############################################################################

set(ITK_TEST_DRIVER "itkTestDriver")

###############################################################################
# The real work on wrappers
###############################################################################

include(ConfigureWrapping.cmake)

###############################################################################
# let the different generators running some code before begining to parse the
# modules
###############################################################################

itk_wrap_modules()

###############################################################################
# Configure specific wrapper modules
###############################################################################

add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/Modules" "${CMAKE_CURRENT_BINARY_DIR}/Modules")


###############################################################################
# let the different generators running some code after have parsed all the
# modules
###############################################################################

itk_end_wrap_modules()

###############################################################################
# Configure WrapITKConfig.cmake and FindWrapITK.cmake for external projects
###############################################################################
# First, configure for use directly from the build dir.
# we must add the options for the generators for the build dir case, so the
# external project won't find all the generators, even the usued ones.
# In the installed case, this feature is useful to let the packagers make several
# subpackages for wrapitk.
set(CONFIG_WRAP_ITK_MORE_OPTIONS)
foreach(lang ${WRAP_ITK_GENERATORS} ${WRAP_ITK_UNUSED_GENERATORS})
  string(TOUPPER "${lang}" lang_upper)
  set(CONFIG_WRAP_ITK_MORE_OPTIONS "${CONFIG_WRAP_ITK_MORE_OPTIONS}set(WRAP_ITK_${lang_upper} ${WRAP_ITK_${lang_upper}} CACHE BOOL \"Build swig ${lang} wrapper support.\")\n")
endforeach()
set(CONFIG_CONFIG_DIR "${WrapITK_SOURCE_DIR}/ConfigurationInputs")
set(CONFIG_CMAKE_DIR "${WrapITK_SOURCE_DIR}")
set(CONFIG_WRAP_ITK_TYPEDEFS_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/Typedefs")
set(CONFIG_WRAP_ITK_INSTALL_LOCATION "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}")
set(CONFIG_WRAP_ITK_MODULES "${WRAP_ITK_MODULES}")
set(CONFIG_ITK_WRAP_PYTHON_MODULE "${WrapITK_BINARY_DIR}/Python/\\\${CTEST_CONFIGURATION_TYPE}")
set(CONFIG_ITK_WRAP_PYTHON_LIBS "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/\\\${CTEST_CONFIGURATION_TYPE}")
set(CONFIG_WRAP_ITK_LIB_DIR  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
configure_file("${WRAP_ITK_CONFIG_DIR}/WrapITKConfig.cmake.in"
               "${WrapITK_BINARY_DIR}/WrapITKConfig.cmake"
               @ONLY)

set(CONFIG_WRAP_ITK_DIR "${WrapITK_BINARY_DIR}")
configure_file("${WRAP_ITK_CONFIG_DIR}/FindWrapITK.cmake.in"
               "${WrapITK_BINARY_DIR}/FindWrapITK.cmake"
               @ONLY)

# Second, configure for installed use. Ensure that no absolute paths
# go into the installed config file, so that it is as portable as possible.
set(CONFIG_WRAP_ITK_MORE_OPTIONS)
set(CONFIG_CONFIG_DIR "\${WrapITK_DIR}/Configuration/Inputs")
set(CONFIG_CMAKE_DIR "\${WrapITK_DIR}/Configuration")
set(CONFIG_WRAP_ITK_TYPEDEFS_DIRECTORY "\${WrapITK_DIR}/Configuration/Typedefs")
set(CONFIG_WRAP_ITK_INSTALL_LOCATION "\${WrapITK_DIR}")
set(CONFIG_ITK_WRAP_PYTHON_MODULE "\${WrapITK_DIR}/Python/")
set(CONFIG_ITK_WRAP_PYTHON_LIBS "\${WrapITK_DIR}/lib")
set(CONFIG_WRAP_ITK_LIB_DIR  "\${WrapITK_DIR}/lib")
configure_file("${WRAP_ITK_CONFIG_DIR}/WrapITKConfig.cmake.in"
               "${WrapITK_BINARY_DIR}/InstallOnly/WrapITKConfig.cmake"
               @ONLY)

set(CONFIG_WRAP_ITK_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}")
configure_file("${WRAP_ITK_CONFIG_DIR}/FindWrapITK.cmake.in"
               "${WrapITK_BINARY_DIR}/InstallOnly/FindWrapITK.cmake"
               @ONLY)
