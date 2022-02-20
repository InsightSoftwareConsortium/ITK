#==========================================================================
#
#   Copyright NumFOCUS
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
cmake_minimum_required(VERSION 3.16.3...3.19.7 FATAL_ERROR)

project(WrapITK)

enable_testing()

###############################################################################
# Configure installation
###############################################################################

if(ITK_INSTALL_PACKAGE_DIR)
  string(REGEX REPLACE "^/" "" path "${ITK_INSTALL_PACKAGE_DIR}/WrapITK/")
else()
  set(path "lib/InsightToolkit/WrapITK/")
endif()
set(WRAP_ITK_INSTALL_PREFIX "${path}" CACHE INTERNAL "subpath where where most of WrapITK files will be installed")

# Output directories.
set(WRAP_ITK_CONFIG_DIR "${WrapITK_SOURCE_DIR}/")
set(WRAP_ITK_CMAKE_DIR "${WrapITK_SOURCE_DIR}")
set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${ITK_DIR}/Wrapping/Typedefs" CACHE INTERNAL "typedefs dir")

if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  # See ITK/CMakeLists.txt for rational for overwriting multi-config default behavior of appending $<CONFIG>
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "$<1:${WrapITK_BINARY_DIR}/lib>" CACHE INTERNAL "Single output directory for building all libraries.")
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

itk_end_wrap_modules_all_generators()


###############################################################################
# Generate Python typehints when wrapped
###############################################################################
if(ITK_WRAP_PYTHON)
  # Generate __init__.pyi file with wrapped templates/interfaces
  set(ITK_STUB_TEMPLATE_IMPORTS "$ENV{ITK_STUB_TEMPLATE_IMPORTS}")
  list(SORT ITK_STUB_TEMPLATE_IMPORTS CASE INSENSITIVE)
  list(JOIN ITK_STUB_TEMPLATE_IMPORTS "\n" ITK_STUB_TEMPLATE_IMPORTS)
  configure_file(Generators/Python/itk/__init__.pyi.in Generators/Python/itk-stubs/__init__.pyi)

  # Generate _proxies.pyi file with wrapped proxy classes and methods
  set(ITK_STUB_PROXY_IMPORTS "$ENV{ITK_STUB_PROXY_IMPORTS}")
  list(SORT ITK_STUB_PROXY_IMPORTS CASE INSENSITIVE)
  list(JOIN ITK_STUB_PROXY_IMPORTS "\n" ITK_STUB_PROXY_IMPORTS)
  configure_file(Generators/Python/itk/_proxies.pyi.in Generators/Python/itk-stubs/_proxies.pyi)
endif()
