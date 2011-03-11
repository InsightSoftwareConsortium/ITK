cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK)

# TODO: Integrate with itk-base and ITK-Common
set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "0")
set(ITK_VERSION_PATCH "0")

set(ITK_INSTALL_PACKAGE_DIR "lib/cmake/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

# Override CMake's built-in add_* commands: assign LABELS to tests and targets
# automatically. Depends on the CMake variable itk-module being set to the
# "current" module when add_* is called.
macro(verify_itk_module_is_set)
  if("" STREQUAL "${itk-module}")
    message(FATAL_ERROR "CMake variable itk-module is not set")
  endif()
endmacro()

macro(add_executable)
  _add_executable(${ARGN})
  verify_itk_module_is_set()
  set_property(TARGET ${ARGV0} PROPERTY LABELS ${itk-module})
  set_property(GLOBAL APPEND PROPERTY ${itk-module}_EXECUTABLE_TARGETS ${ARGV0})
endmacro()

macro(add_library)
  _add_library(${ARGN})
  verify_itk_module_is_set()
  set_property(TARGET ${ARGV0} PROPERTY LABELS ${itk-module})
  set_property(GLOBAL APPEND PROPERTY ${itk-module}_LIBRARY_TARGETS ${ARGV0})
endmacro()

macro(add_test)
  _add_test(${ARGN})
  verify_itk_module_is_set()
  if("NAME" STREQUAL "${ARGV0}")
    set(_imm_test ${ARGV1})
  else()
    set(_imm_test ${ARGV0})
  endif()
  set_property(TEST ${_imm_test} PROPERTY LABELS ${itk-module})
  set_property(GLOBAL APPEND PROPERTY ${itk-module}_TESTS ${_imm_test})
endmacro()

include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
include(${ITK_SOURCE_DIR}/CMake/ITKPlatformSpecificChecks.cmake)

set(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake)

include(CTest)

# Setup build locations.
if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/bin)
endif()
if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/lib)
endif()
if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/lib)
endif()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${ITK_BINARY_DIR}/root CACHE PATH "" FORCE)
  set(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT 0)
endif()

set(itk-base_DIR ${ITK_SOURCE_DIR}/CMake/itk-base)
add_subdirectory(${itk-base_DIR})
find_package(itk-base 4.0 EXACT REQUIRED)


#-----------------------------------------------------------------------------
# ITK build configuration options.
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
set(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

set(ITK_USE_MODULAR_BUILD ON)

# ITK build classes that are in the review process
option(ITK_USE_REVIEW "Build algorithms that are candidates to be included in the Toolkit. This classes are not covered by the Backward Compatibility Policy, no
r the Copyright Policy of ITK." OFF)
mark_as_advanced(ITK_USE_REVIEW)

if (ITK_USE_REVIEW)
  set(Module_ITK-Review ON)
endif(ITK_USE_REVIEW)

option(ITK_USE_GPU "GPU acceleration via OpenCL" OFF)
mark_as_advanced(ITK_USE_GPU)


set(EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
set(ITK_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")

#----------------------------------------------------------------------------
configure_file(CMake/CTestCustom.cmake.in CTestCustom.cmake)

set(ITK_TEST_OUTPUT_DIR "${ITK_BINARY_DIR}/Testing/Temporary")

# Find the data directory.
find_path(ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})
if(NOT ITK_DATA_ROOT)
  if(BUILD_TESTING)
    message(WARNING "ITK_DATA_ROOT was not found.  ctest will not run properly.")
  endif()
endif()
mark_as_advanced(ITK_DATA_ROOT)

#CSWIG related
set(ITKWISH_EXECUTABLE "${CXX_TEST_PATH}/itkwish")

if(ITK_CSWIG_TCL)
  find_program(ITKWISH_EXECUTABLE itkwish ${ITK_DIR}/bin ${ITK_DIR})
endif()

mark_as_advanced(TCL_TCLSH)

set(ITKTesting_BINARY_DIR "${ITK_BINARY_DIR}/Testing")

option(ITK_USE_FLOATINGPOINTEXCEPTIONS
      "Enable Floating Point Exceptions for Testing" OFF)
mark_as_advanced(ITK_USE_FLOATINGPOINTEXCEPTIONS)

#This flag is used in particular, to enable some tests that require large memory to run.
# This probably can be discovered at configuration time by CMake. (Check with CMake developers).
set(ITK_COMPUTER_MEMORY_SIZE 1 CACHE STRING "Provide here the size of your RAM Memory in GigaBytes")
mark_as_advanced(ITK_COMPUTER_MEMORY_SIZE)



#----------------------------------------------------------------------
# Load the module DAG.
set(ITK_MODULES_ALL)
file(GLOB meta RELATIVE "${ITK_SOURCE_DIR}"
   "${ITK_SOURCE_DIR}/*/*/*/itk-module.cmake" # grouped modules
  )
foreach(f ${meta})
  include(${ITK_SOURCE_DIR}/${f})
  list(APPEND ITK_MODULES_ALL ${itk-module})
  get_filename_component(${itk-module}_BASE ${f} PATH)
  set(${itk-module}_SOURCE_DIR ${ITK_SOURCE_DIR}/${${itk-module}_BASE})
  set(${itk-module}_BINARY_DIR ${ITK_BINARY_DIR}/${${itk-module}_BASE})
  if(BUILD_TESTING AND EXISTS ${${itk-module}_SOURCE_DIR}/test)
    list(APPEND ITK_MODULES_ALL ${itk-module-test})
    set(${itk-module-test}_SOURCE_DIR ${${itk-module}_SOURCE_DIR}/test)
    set(${itk-module-test}_BINARY_DIR ${${itk-module}_BINARY_DIR}/test)
    set(${itk-module-test}_IS_TEST 1)
    set(${itk-module}_TESTED_BY ${itk-module-test})
  endif()
endforeach()

#----------------------------------------------------------------------
# Provide module selections by groups
include(${ITK_SOURCE_DIR}/CMake/ITKGroups.cmake)


# Provide an option for every module.
if("$ENV{DASHBOARD_TEST_FROM_CTEST}" STREQUAL "")
  # developer build
  set(ITK_BUILD_ALL_MODULES_DEFAULT OFF)
else()
  # dashboard build
  set(ITK_BUILD_ALL_MODULES_DEFAULT ON)
endif()
option(ITK_BUILD_ALL_MODULES "Request to build all modules"
  ${ITK_BUILD_ALL_MODULES_DEFAULT})
mark_as_advanced(ITK_BUILD_ALL_MODULES)

foreach(itk-module ${ITK_MODULES_ALL})
  if(NOT ${itk-module}_IS_TEST)
    option(Module_${itk-module} "Request building ${itk-module}" "${ITK_MODULE_${itk-module}_DEFAULT}")
    mark_as_advanced(Module_${itk-module})
  endif()
endforeach()

# Follow dependencies.
macro(itk_module_enable itk-module _needed_by)
  if(NOT Module_${itk-module})
    if(NOT ${itk-module}_TESTED_BY OR
      NOT "x${_needed_by}" STREQUAL "x${${itk-module}_TESTED_BY}")
      list(APPEND ITK_MODULE_${itk-module}_NEEDED_BY ${_needed_by})
    endif()
  endif()
  if(NOT ${itk-module}_ENABLED)
    set(${itk-module}_ENABLED 1)
    foreach(dep IN LISTS ITK_MODULE_${itk-module}_DEPENDS)
      itk_module_enable(${dep} ${itk-module})
    endforeach()
    if(${itk-module}_TESTED_BY)
      itk_module_enable(${${itk-module}_TESTED_BY} "")
    endif()
  endif()
endmacro()

foreach(itk-module ${ITK_MODULES_ALL})
  if(Module_${itk-module} OR ITK_BUILD_ALL_MODULES)
    itk_module_enable("${itk-module}" "")
  elseif(ITK_MODULE_${itk-module}_REQUEST_BY)
    itk_module_enable("${itk-module}" "${ITK_MODULE_${itk-module}_REQUEST_BY}")
  endif()
endforeach()

# Build final list of enabled modules.
set(ITK_MODULES_ENABLED "")
set(ITK_MODULES_DISABLED "")
foreach(itk-module ${ITK_MODULES_ALL})
  if(${itk-module}_ENABLED)
    list(APPEND ITK_MODULES_ENABLED ${itk-module})
  else()
    list(APPEND ITK_MODULES_DISABLED ${itk-module})
  endif()
endforeach()
list(SORT ITK_MODULES_ENABLED) # Deterministic order.
list(SORT ITK_MODULES_DISABLED) # Deterministic order.

# Order list to satisfy dependencies.
include(CMake/TopologicalSort.cmake)
topological_sort(ITK_MODULES_ENABLED ITK_MODULE_ _DEPENDS)

set(ITK_MODULES_DISABLED_CPACK )
foreach(m ${ITK_MODULES_DISABLED})
  list(APPEND ITK_MODULES_DISABLED_CPACK "/${m}/")
endforeach()
set(CPACK_SOURCE_IGNORE_FILES
  "${ITK_MODULES_DISABLED_CPACK};/\\\\.git")
include(CPack)

# Report what will be built.
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    if(Module_${itk-module})
      set(_reason ", requested by Module_${itk-module}")
    elseif(ITK_BUILD_ALL_MODULES)
      set(_reason ", requested by ITK_BUILD_ALL_MODULES")
    else()
      set(_reason ", needed by [${ITK_MODULE_${itk-module}_NEEDED_BY}]")
    endif()
    message(STATUS "Enabled ${itk-module}${_reason}.")
  endif()
endforeach()

#-----------------------------------------------------------------------------
# Write a Project.xml file to send the description of the submodules and
# their dependencies up to CDash:

set(main_project_name "ITKModular")

if(ITK_GENERATE_PROJECT_XML)
  set(filename "${ITK_BINARY_DIR}/${main_project_name}.Project.xml")

  set(xml "<?xml version='1.0' encoding='UTF-8'?>\n")
  set(xml "${xml}<Project name='${main_project_name}'>\n")
  foreach(module ${ITK_MODULES_ENABLED})
    set(xml "${xml}  <SubProject name='${module}'>\n")
    set(deps "")
    foreach(dep ${ITK_MODULE_${module}_DEPENDS})
      set(xml "${xml}    <Dependency name='${dep}'/>\n")
    endforeach()
    set(xml "${xml}  </SubProject>\n")
  endforeach()
  set(xml "${xml}</Project>\n")

  # Always write out "${filename}.in":
  file(WRITE ${filename}.in "${xml}")

  # Use configure_file so "${filename}" only changes when its content changes:
  configure_file(${filename}.in ${filename} COPYONLY)
endif()

#-----------------------------------------------------------------------------

# Write the list of enabled modules out for ctest scripts to use as an
# in-order subproject list:

if(ITK_GENERATE_SUBPROJECTS_CMAKE)
  set(filename "${ITK_BINARY_DIR}/${main_project_name}.SubProjects.cmake")

  set(s "# Generated by CMake, do not edit!\n")
  set(s "${s}set(itk_subprojects\n")
  foreach(itk-module ${ITK_MODULES_ENABLED})
    if(NOT ${itk-module}_IS_TEST)
      set(s "${s}  \"${itk-module}\"\n")
    endif()
  endforeach()
  set(s "${s})\n")

  # Always write out "${filename}.in":
  file(WRITE ${filename}.in "${s}")

  # Use configure_file so "${filename}" only changes when its content changes:
 configure_file(${filename}.in ${filename} COPYONLY)
endif()

#-----------------------------------------------------------------------------

if(NOT ITK_MODULES_ENABLED)
  message(WARNING "No modules enabled!")
  file(REMOVE "${ITK_BINARY_DIR}/ITKTargets.cmake")
  return()
endif()

file(WRITE "${ITK_BINARY_DIR}/ITKTargets.cmake"
  "# Generated by CMake, do not edit!")

macro(init_module_vars)
  verify_itk_module_is_set()
  set(${itk-module}-targets ITKTargets)
  set(${itk-module}-targets-install "${ITK_INSTALL_PACKAGE_DIR}/ITKTargets.cmake")
  set(${itk-module}-targets-build "${ITK_BINARY_DIR}/ITKTargets.cmake")
  set(${itk-module}_DIR "${${itk-module}_BINARY_DIR}") # find_package

  # Start each module with these properties empty. Calls to add_executable,
  # add_library and add_test accumulate names into these properties:
  set_property(GLOBAL PROPERTY ${itk-module}_EXECUTABLE_TARGETS "")
  set_property(GLOBAL PROPERTY ${itk-module}_LIBRARY_TARGETS "")
  set_property(GLOBAL PROPERTY ${itk-module}_TESTS "")
endmacro()

macro(add_module_target)
  verify_itk_module_is_set()
  add_custom_target(${itk-module}-all)
  get_property(_imm_exe_targets GLOBAL PROPERTY ${itk-module}_EXECUTABLE_TARGETS)
  get_property(_imm_lib_targets GLOBAL PROPERTY ${itk-module}_LIBRARY_TARGETS)
  set(_imm_dep_targets ${_imm_exe_targets} ${_imm_lib_targets})
  if(_imm_dep_targets)
    add_dependencies(${itk-module}-all ${_imm_dep_targets})
  endif()
endmacro()

# Build all modules.
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    init_module_vars()
  endif()
  add_subdirectory("${${itk-module}_SOURCE_DIR}" "${${itk-module}_BINARY_DIR}")
endforeach()
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    add_module_target()
  endif()
endforeach()

#-----------------------------------------------------------------------------

# Create list of available modules and libraries.
set(ITK_CONFIG_MODULES_ENABLED "")
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    list(APPEND ITK_CONFIG_MODULES_ENABLED ${itk-module})
  endif()
endforeach()

# Generate ITKConfig.cmake for the build tree.f
set(ITK_CONFIG_CODE "# Module directories")
foreach(itk-module ${ITK_CONFIG_MODULES_ENABLED})
  set(ITK_CONFIG_CODE "${ITK_CONFIG_CODE}
set(${itk-module}_DIR \"${${itk-module}_DIR}\")")
endforeach()
set(ITK_USE_FILE "${ITK_SOURCE_DIR}/CMake/UseITK.cmake")
configure_file(CMake/ITKConfig.cmake.in ITKConfig.cmake @ONLY)

# Generate ITKConfig.cmake for the install tree.
set(ITK_CONFIG_CODE "
# Compute the installation prefix from this ITKConfig.cmake file location.
get_filename_component(ITK_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
# Construct the proper number of get_filename_component(... PATH)
# calls to compute the installation prefix.
string(REGEX REPLACE "/" ";" _count "${ITK_INSTALL_PACKAGE_DIR}")
foreach(p ${_count})
  set(ITK_CONFIG_CODE "${ITK_CONFIG_CODE}
get_filename_component(ITK_INSTALL_PREFIX \"\${ITK_INSTALL_PREFIX}\" PATH)")
endforeach(p)
foreach(itk-module ${ITK_CONFIG_MODULES_ENABLED})
  set(ITK_CONFIG_CODE "${ITK_CONFIG_CODE}
set(${itk-module}_DIR \"\${ITK_INSTALL_PREFIX}/lib/cmake/${itk-module}-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\")")
endforeach()
set(ITK_USE_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/UseITK.cmake")
configure_file(CMake/ITKConfig.cmake.in CMakeFiles/ITKConfig.cmake @ONLY)

#-----------------------------------------------------------------------------
configure_file(CMake/ITKConfigVersion.cmake.in ITKConfigVersion.cmake @ONLY)

install(FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKConfig.cmake
              ${ITK_BINARY_DIR}/ITKConfigVersion.cmake
              CMake/UseITK.cmake
  DESTINATION ${ITK_INSTALL_PACKAGE_DIR})
install(EXPORT ITKTargets DESTINATION ${ITK_INSTALL_PACKAGE_DIR})
