#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
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

# Set the module prefix once upon loading this files
# Case 1:  An external module is loading this file from
#          ${ITK_SOURCE_DIR}/CMake/ITKModuleExternal.cmake
# Case 2:  ITK is including this file from
#          ${ITK_SOURCE_DIR}/CMakeLists.txt
# module_prefix is a global variable
if(EXTERNAL_WRAP_ITK_PROJECT)
  set(module_prefix ${itk-module})
else()
  set(module_prefix ITK)
endif()

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

unset(GLOBAL_IdxFilesList)
unset(GLOBAL_IdxFilesList CACHE)

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

# See https://peps.python.org/pep-0561/#stub-only-packages
# Packaging typehints with package so use "itk", if making separate typehint
# package use "itk-stubs"
set(ITK_STUB_BASENAME "itk")
# ITK_STUB_DIR: all stub files are stored in this directory
set(ITK_STUB_DIR "${ITK_DIR}/Wrapping/Generators/Python/${ITK_STUB_BASENAME}" CACHE INTERNAL "where python interface files are stored.")
file(MAKE_DIRECTORY ${ITK_STUB_DIR})

# ITK_PKL_DIR: all temporary pickled object AST representations stored here
set(ITK_PKL_DIR "${ITK_DIR}/Wrapping/Generators/Python/itk-pkl" CACHE INTERNAL "where temp pkl files are stored")
file(MAKE_DIRECTORY ${ITK_PKL_DIR})
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
# Configure specific wrapper modules
###############################################################################

unset(WRAP_ITK_MODULES CACHE)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Modules/)
if(ITK_SOURCE_DIR)
  foreach(_module_enabled ${ITK_CONFIG_MODULES_ENABLED})
    if(EXISTS "${${_module_enabled}_SOURCE_DIR}/wrapping/CMakeLists.txt")
      list(APPEND WRAP_ITK_MODULES ${_module_enabled})
      set(itk-module_SOURCE_DIR ${${_module_enabled}_SOURCE_DIR})
      add_subdirectory("${${_module_enabled}_SOURCE_DIR}/wrapping" ${CMAKE_CURRENT_BINARY_DIR}/Modules/${_module_enabled})
      unset(itk-module_SOURCE_DIR)
    endif()
  endforeach()
else() # Building a module externally so itk-module variable must have been previously set
  if(EXISTS "${${itk-module}_SOURCE_DIR}/wrapping/CMakeLists.txt")
    foreach(_module_depend ${ITK_MODULE_${itk-module}_DEPENDS})
      if(EXISTS ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module_depend}.mdx)
        list(APPEND WRAP_ITK_MODULES ${_module_depend})
      endif()
    endforeach()
    list(APPEND WRAP_ITK_MODULES ${itk-module})
    add_subdirectory("${${itk-module}_SOURCE_DIR}/wrapping" ${CMAKE_CURRENT_BINARY_DIR}/Modules/${itk-module})
  endif()
endif()
set(WRAP_ITK_MODULES ${WRAP_ITK_MODULES} CACHE INTERNAL "Internal library list.")

###############################################################################
# let the different generators running some code after have parsed all the
# modules
###############################################################################

if(${module_prefix}_WRAP_PYTHON)
  # Wrap PyUtils
  if(NOT EXTERNAL_WRAP_ITK_PROJECT)
    add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyUtils)
    add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyBase)
  endif()
endif()


###############################################################################
# Generate Python typehints when wrapped
###############################################################################
if(ITK_WRAP_PYTHON)
  # Generate .pyi files for each class
  # The intent is that the pyi_generator file is run after every occurrence of igenerator has ran.
  # The call to igenerator is contained within a macro that is called by each of the modules.
  # According to online documents CMAkE dependencies can only be created between
  # custom_targets and custom_commands contained within the same CMakeLists file. It is unknown if this
  # can be done since the macros are called from a variety of locations.

  # PYI_GENERATOR: The file location of the pyi_generator.py script
  #   This script is responsible for merging all of the pickle files together and
  #   generating the Template and Proxy files for each class. In addition this file
  #   generates the __init__.pyi and _proxies.pyi files used to interface between
  #   the individual .pyi files.
  set(PYI_GENERATOR  "${CMAKE_CURRENT_SOURCE_DIR}/Generators/Python/itk/pyi_generator.py")

  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/GlobalIdxFilesList.txt.in
                 ${CMAKE_CURRENT_BINARY_DIR}/GlobalIdxFilesList.txt)

  # Run pyi_generator
  # ${GLOBAL_IdxFilesList}: is described within SwigInterface/CMakeLists.txt
  #   The variable is passed to pyi_generator to make sure that the function only uses current index files
  #   All index files found that are not in the given list are assumed to be outdated and are removed.
  add_custom_target(itk-stub-files ALL
          BYPRODUCTS "${ITK_STUB_DIR}/_proxies.pyi" "${ITK_STUB_DIR}/__init__.pyi"
          COMMAND ${Python3_EXECUTABLE} ${PYI_GENERATOR}
            --pyi_dir "${ITK_STUB_DIR}"
            --pkl_dir "${ITK_PKL_DIR}"
            --index_list_file "${CMAKE_CURRENT_BINARY_DIR}/GlobalIdxFilesList.txt"
          DEPENDS ${PYI_GENERATOR} ${GLOBAL_IdxFilesList}
          COMMENT "Generating .pyi files for Python wrapper interface"
          VERBATIM
  )

  # Add module dependencies to ensure .index.txt files have been generated
  UNIQUE(WRAPPED_MODULE_LIST "${WRAP_ITK_MODULES};ITKPyUtils")
  foreach(WRAPPER_LIBRARY_NAME ${WRAPPED_MODULE_LIST})
    list(LENGTH ${WRAPPER_LIBRARY_NAME}PyiIdxFiles ${WRAPPER_LIBRARY_NAME}PyiIdxFiles_Len)
    if(${WRAPPER_LIBRARY_NAME}PyiIdxFiles_Len GREATER 0)
      # <module>Swig custom targets build .index.txt files as byproducts
      add_dependencies(itk-stub-files ${WRAPPER_LIBRARY_NAME}Swig)
    endif()
  endforeach()

  unset(ITK_STUB_DIR CACHE)
  unset(ITK_PKL_DIR CACHE)
endif()
