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

cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)
foreach(p
    CMP0025 # CMake 3.0
    CMP0042 # CMake 3.0
    CMP0054 # CMake 3.1
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

project(WrapITK)

include("CMakeUtilityFunctions.cmake")

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

option(ITK_WRAP_complex_float "Wrap complex< float > type" ON)
option(ITK_WRAP_complex_double "Wrap complex< double > type" OFF)

# Check for type conditions that need to be fullfilled.
foreach(t float double)
  # Vectors
  if(ITK_WRAP_vector_${t} AND NOT ITK_WRAP_${t})
    message(SEND_ERROR "To use 'ITK_WRAP_vector_${t}', please set 'ITK_WRAP_${t}' to ON")
  endif()

  # Covariant vectors
  if(ITK_WRAP_covariant_vector_${t} AND NOT ITK_WRAP_${t})
    message(SEND_ERROR "To use 'ITK_WRAP_covariant_vector_${t}', please set 'ITK_WRAP_${t}' to ON")
  endif()

  # Complex
  if(ITK_WRAP_complex_${t} AND NOT ITK_WRAP_${t})
    message(SEND_ERROR "To use 'ITK_WRAP_complex_${t}', please set 'ITK_WRAP_${t}' to ON")
  endif()
endforeach()

foreach(t unsigned_char unsigned_short)
  if(ITK_WRAP_rgb_${t} AND NOT ITK_WRAP_${t})
    message(SEND_ERROR "To use 'ITK_WRAP_rgb_${t}', please set 'ITK_WRAP_${t}' to ON")
  endif()

  if(ITK_WRAP_rgba_${t} AND NOT ITK_WRAP_${t})
    message(SEND_ERROR "To use 'ITK_WRAP_rgba_${t}', please set 'ITK_WRAP_${t}' to ON")
  endif()
endforeach()

if(DEFINED ITK_WRAP_DIMS)
  # Keep ITK_WRAP_DIMS for backward compatibility.
  set(ITK_WRAP_IMAGE_DIMS "${ITK_WRAP_DIMS}" CACHE STRING "Dimensions available separated by semicolons (;)")
else()
  set(ITK_WRAP_IMAGE_DIMS "2;3" CACHE STRING "Dimensions available separated by semicolons (;)")
  # Keep ITK_WRAP_DIMS for extra modules developed outside of ITK.
  set(ITK_WRAP_DIMS "${ITK_WRAP_IMAGE_DIMS}")
endif()
UNIQUE(vector_components "${ITK_WRAP_IMAGE_DIMS};4")
set(ITK_WRAP_VECTOR_COMPONENTS "${vector_components}" CACHE STRING "Number of vector components available separated by semicolons (;)")

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
set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${ITK_DIR}/Wrapping/Typedefs" CACHE INTERNAL "typedefs dir")

if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${WrapITK_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
endif()

if(NOT EXECUTABLE_OUTPUT_PATH)
  set(EXECUTABLE_OUTPUT_PATH ${WrapITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
endif()

# Set WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER to a non-null value, like
# "Wrapping", which will be inserted into the wrapping install component name.
# This can be used to split installation package components.
if(NOT WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER)
  set(WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER "")
endif()

# Set WRAP_ITK_INSTALL_COMPONENT_PER_MODULE to 1 to have wrapping install
# component names prefixed with their respective module name.
# This can be used to have fine-control over the split of installation.
if(NOT DEFINED WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
  set(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE 0)
endif()

mark_as_advanced(CMAKE_LIBRARY_OUTPUT_DIRECTORY EXECUTABLE_OUTPUT_PATH WRAP_ITK_INSTALL_PREFIX)

set(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

###############################################################################
# Additional files for installation
###############################################################################

if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  # Install CMake module(s) along with ITKCommon wrapping
  set(_component_module "")
  if(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
    set(_component_module "ITKCommon")
  endif()
  install(FILES "${WrapITK_BINARY_DIR}/InstallOnly/WrapITKConfig.cmake"
    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/"
    COMPONENT ${_component_module}${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}Development
    )
endif()

set(WRAP_ITK_TYPEDEFS_DIRECTORY "${ITK_DIR}/Wrapping/Typedefs")
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
# Configure WrapITKConfig.cmake for external projects
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
set(CONFIG_WRAP_ITK_INSTALL_LOCATION "${WRAP_ITK_INSTALL_PREFIX}")
set(CONFIG_WRAP_ITK_MODULES "${WRAP_ITK_MODULES}")
set(CONFIG_ITK_WRAP_PYTHON_MODULE "${WrapITK_BINARY_DIR}/Python/\\\${CTEST_CONFIGURATION_TYPE}")
set(CONFIG_ITK_WRAP_PYTHON_LIBS "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/\\\${CTEST_CONFIGURATION_TYPE}")
set(CONFIG_WRAP_ITK_LIB_DIR  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
configure_file("${WRAP_ITK_CONFIG_DIR}/WrapITKConfig.cmake.in"
               "${WrapITK_BINARY_DIR}/WrapITKConfig.cmake"
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
