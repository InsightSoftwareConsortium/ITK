cmake_minimum_required(VERSION 2.8.4 FATAL_ERROR)
include(CMakeDependentOption)
#
# use ExternalProject
include(ExternalProject)

if(WIN32)
  cmake_minimum_required(VERSION 2.8.8)
elseif(NOT EXISTS /dev/urandom)
  cmake_minimum_required(VERSION 2.8.5)
endif()

project(ITK)

if( CMAKE_HOST_WIN32 )

  string( LENGTH "${CMAKE_CURRENT_SOURCE_DIR}" n )
  if( n GREATER 50 )
    message(
      FATAL_ERROR
      "ITK source code directory path length is too long (${n} > 50)."
      "Please move the ITK source code directory to a directory with a shorter path."
      )
  endif()

  string( LENGTH "${CMAKE_CURRENT_BINARY_DIR}" n )
  if( n GREATER 50 )
    message(
      FATAL_ERROR
      "ITK build directory path length is too long (${n} > 50)."
      "Please set the ITK build directory to a directory with a shorter path."
      )
  endif()

endif()

set(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
include(PreventInSourceBuilds)
include(PreventInBuildInstalls)
include(ITKModuleMacros)
include(ITKExternalData)
include(itkCheckSourceTree)

set(main_project_name ${_ITKModuleMacros_DEFAULT_LABEL})

#-----------------------------------------------------------------------------
# ITK version number.
set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "4")
set(ITK_VERSION_PATCH "0")

if(NOT ITK_INSTALL_RUNTIME_DIR)
  set(ITK_INSTALL_RUNTIME_DIR bin)
endif()
if(NOT ITK_INSTALL_LIBRARY_DIR)
  set(ITK_INSTALL_LIBRARY_DIR lib)
endif()
if(NOT ITK_INSTALL_ARCHIVE_DIR)
  set(ITK_INSTALL_ARCHIVE_DIR lib)
endif()
if(NOT ITK_INSTALL_INCLUDE_DIR)
  set(ITK_INSTALL_INCLUDE_DIR include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_DATA_DIR)
  set(ITK_INSTALL_DATA_DIR share/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_DOC_DIR)
  set(ITK_INSTALL_DOC_DIR share/doc/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_PACKAGE_DIR)
  set(ITK_INSTALL_PACKAGE_DIR "lib/cmake/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
endif()

# Override CMake's built-in add_* commands: assign LABELS to tests and targets
# automatically. Depends on the CMake variable itk-module being set to the
# "current" module when add_* is called.
macro(verify_itk_module_is_set)
  if("" STREQUAL "${itk-module}")
    message(FATAL_ERROR "CMake variable itk-module is not set")
  endif()
endmacro()

#-----------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()


#-----------------------------------------------------------------------------
# Enable runinng cppcheck for each module on its source and test directories.
option(ITK_CPPCHECK_TEST "Run cppcheck for static code analysis" OFF)
mark_as_advanced(ITK_CPPCHECK_TEST)

#-----------------------------------------------------------------------------
# Default values for CMake options may vary based on whether it's a
# dashboard build or not:
#
if("$ENV{DASHBOARD_TEST_FROM_CTEST}" STREQUAL "")
  # developer build
  set(ITK_BUILD_ALL_MODULES_DEFAULT ON)
  set(BUILD_EXAMPLES_DEFAULT ON)
else()
  # dashboard build
  set(ITK_BUILD_ALL_MODULES_DEFAULT ON)
  set(BUILD_EXAMPLES_DEFAULT ON)
endif()

#-----------------------------------------------------------------------------
# Enable the download and use of BrainWeb datasets.
# When this data is available, additional 3D tests are enabled.
option(ITK_USE_BRAINWEB_DATA "Download and use BrainWeb data for advanced testing" OFF)
mark_as_advanced(ITK_USE_BRAINWEB_DATA)

#-----------------------------------------------------------------------------
# ITK wrapper for add_test that automatically sets the test's LABELS property
# to the value of its containing module.
#
function(itk_add_test)
  # Add tests with data in the ITKData group.
  ExternalData_add_test(ITKData ${ARGN})

  if("NAME" STREQUAL "${ARGV0}")
    set(_iat_testname ${ARGV1})
  else()
    set(_iat_testname ${ARGV0})
  endif()

  if(itk-module)
    set(_label ${itk-module})
    if(TARGET ${itk-module}-all AND "${ARGN}" MATCHES "DATA{")
      add_dependencies(${itk-module}-all ITKData)
    endif()
  else()
    set(_label ${main_project_name})
  endif()

  set_property(TEST ${_iat_testname} PROPERTY LABELS ${_label})
endfunction()

#-----------------------------------------------------------------------------
# ITK build configuration options.
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
set(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

include(ITKSetStandardCompilerFlags)
#---------------------------------------------------------------
# run try compiles and tests for ITK
include(itkTestFriendTemplatedFunction)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")

include(CTest)

include( CppcheckTargets )

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
set(ITK_MODULES_DIR "${ITK_BINARY_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules")


#-----------------------------------------------------------------------------
# Provide compatibility options.
option(ITKV3_COMPATIBILITY "Enable compatibility with ITK3.x when possible." OFF)
option(ITK_LEGACY_REMOVE "Remove all legacy code completely." OFF)
option(ITK_LEGACY_SILENT "Silence all legacy code messages." OFF)
# The ITK_FUTURE_LEGACY_REMOVE is a very advanced feature only
# available to developers at configuration time by manually
# adding this to the CMakeCache.txt -DITK_FUTURE_LEGACY_REMOVE:BOOL=ON
set( ITK_FUTURE_LEGACY_REMOVE 0 CACHE INTERNAL "Remove code that will become legacy in future releases completely." )
mark_as_advanced(ITK_LEGACY_SILENT ITK_LEGACY_REMOVE ITK_V3_COMPATIBILITY)

# The disabling of legacy code, and the ITKv3 compatibility option can not both be
# requested.  If removal of legacy code is requested, then ITKV3_COMPATIBILITY must
# be off.
if(ITKV3_COMPATIBILITY AND ITK_LEGACY_REMOVE)
  message(FATAL_ERROR "Invlaid configuration: ITKV3_COMPATIBILITY AND ITK_LEGACY_REMOVE can not both be ON")
endif()

#-----------------------------------------------------------------------------
# ITK build classes that are in the review process
option(ITK_USE_REVIEW "Build algorithms that are candidates to be included in the Toolkit. This classes are not covered by the Backward Compatibility Policy, no
r the Copyright Policy of ITK." OFF)
mark_as_advanced(ITK_USE_REVIEW)

if (ITK_USE_REVIEW)
  set(Module_ITKReview ON)
endif()

#-----------------------------------------------------------------------------
# ITK uses KWStyle for checking the coding style
include(${ITK_SOURCE_DIR}/Utilities/KWStyle/KWStyle.cmake)

#-----------------------------------------------------------------------------
# ITK build the Examples that are illustrated in the Software Guide
option(BUILD_EXAMPLES "Build the Examples directory."
  ${BUILD_EXAMPLES_DEFAULT})

#-----------------------------------------------------------------------------
# Enable GPU support. Requires OpenCL to be installed
option(ITK_USE_GPU "GPU acceleration via OpenCL" OFF)
mark_as_advanced(ITK_USE_GPU)

if( ITK_USE_GPU )
   include(itkOpenCL)
endif()

#-----------------------------------------------------------------------------
# Manage FFT v3 Options
#
# ITK_USE_FFTWD -- use double precision fftw
if(DEFINED USE_FFTWD)
  set(ITK_USE_FFTWD_DEFAULT ${USE_FFTWD})
else()
  set(ITK_USE_FFTWD_DEFAULT OFF)
endif()
option(ITK_USE_FFTWD "Use double precision fftw if found" ${ITK_USE_FFTWD_DEFAULT})
mark_as_advanced(ITK_USE_FFTWD)
#
# ITK_USE_FFTWF -- use single precision fftw
if(DEFINED USE_FFTWF)
  set(ITK_USE_FFTWF_DEFAULT ${USE_FFTWF})
else()
  set(ITK_USE_FFTWF_DEFAULT OFF)
endif()
option(ITK_USE_FFTWF "Use single precision fftw if found" ${ITK_USE_FFTWF_DEFAULT})
mark_as_advanced(ITK_USE_FFTWF)

# ITK_USE_SYSTEM_FFTW -- locate a readybuilt fftw installation
if(DEFINED USE_SYSTEM_FFTW)
  set(ITK_USE_SYSTEM_FFTW_DEFAULT ${USE_SYSTEM_FFTW})
else()
  set(ITK_USE_SYSTEM_FFTW_DEFAULT OFF)
endif()
option(ITK_USE_SYSTEM_FFTW "Use an installed version of fftw" ${ITK_USE_SYSTEM_FFTW_DEFAULT})
mark_as_advanced(ITK_USE_SYSTEM_FFTW)


if( ITK_USE_FFTWD OR ITK_USE_FFTWF )
  include(itkExternal_FFTW)
endif()

if(NOT ITKV3_COMPATIBILITY)
  option(ITK_USE_64BITS_IDS "When ON, ITK will use 64 bits integers to index pixels. This is needed for managing images larger than 4Gb in some platforms." OFF)
  mark_as_advanced(ITK_USE_64BITS_IDS)
endif()

# ITK turn on concept checking
option(ITK_USE_CONCEPT_CHECKING "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter." ON)
mark_as_advanced(ITK_USE_CONCEPT_CHECKING)
option(ITK_USE_STRICT_CONCEPT_CHECKING "Turn on Strict concept checking to give more stringent errors at compile time if a type cannot be used as a template parameter." OFF)
mark_as_advanced(ITK_USE_STRICT_CONCEPT_CHECKING)

#----------------------------------------------------------------------------
set(ITK_TEST_OUTPUT_DIR "${ITK_BINARY_DIR}/Testing/Temporary")

# Configure the default ITK_DATA_ROOT for the location of ITK Data.
set(ITK_DATA_ROOT ${ITK_SOURCE_DIR}/Testing/Data)

# Location of ITK Example Data.
set(ITK_EXAMPLE_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")

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
    set(${itk-module-test}_TESTS_FOR ${itk-module})
  endif()

  # Reject bad dependencies.
  string(REGEX MATCHALL ";(ITKDeprecated|ITKReview|ITKIntegratedTest);"
    _bad_deps ";${ITK_MODULE_${itk-module}_DEPENDS};${ITK_MODULE_${itk-module-test}_DEPENDS};")
  foreach(dep ${_bad_deps})
    if(NOT "${itk-module}" MATCHES "^(${dep}|ITKIntegratedTest)$")
      message(FATAL_ERROR
        "Module \"${itk-module}\" loaded from\n"
        "  ${${itk-module}_BASE}/itk-module.cmake\n"
        "may not depend on module \"${dep}\".")
    endif()
  endforeach()
endforeach()
# Clear variables set later in each module.
unset(itk-module)
unset(itk-module-test)

# Validate the module DAG.
macro(itk_module_check itk-module _needed_by stack)
  if(NOT ITK_MODULE_${itk-module}_DECLARED)
    message(FATAL_ERROR "No such module \"${itk-module}\" needed by \"${_needed_by}\"")
  endif()
  if(check_started_${itk-module} AND NOT check_finished_${itk-module})
    # We reached a module while traversing its own dependencies recursively.
    set(msg "")
    foreach(entry ${stack})
      set(msg " ${entry} =>${msg}")
      if("${entry}" STREQUAL "${itk-module}")
        break()
      endif()
    endforeach()
    message(FATAL_ERROR "Module dependency cycle detected:\n ${msg} ${itk-module}")
  elseif(NOT check_started_${itk-module})
    # Traverse dependencies of this module.  Mark the start and finish.
    set(check_started_${itk-module} 1)
    foreach(dep IN LISTS ITK_MODULE_${itk-module}_DEPENDS)
      itk_module_check(${dep} ${itk-module} "${itk-module};${stack}")
    endforeach()
    set(check_finished_${itk-module} 1)
  endif()
endmacro()

foreach(itk-module ${ITK_MODULES_ALL})
  itk_module_check("${itk-module}" "" "")
endforeach()

#----------------------------------------------------------------------

# Provide an option for all modules.
option(ITK_BUILD_ALL_MODULES "Request to build all modules"
  ${ITK_BUILD_ALL_MODULES_DEFAULT})

# Provide module selections by groups
include(${ITK_SOURCE_DIR}/CMake/ITKGroups.cmake)

# Provide an option for each module.
foreach(itk-module ${ITK_MODULES_ALL})
  if(NOT ${itk-module}_IS_TEST)
    option(Module_${itk-module} "Request building ${itk-module}" OFF)
    mark_as_advanced(Module_${itk-module})
    if(ITK_MODULE_${itk-module}_EXCLUDE_FROM_ALL)
      set(ITK_MODULE_${itk-module}_IN_ALL 0)
    else()
      set(ITK_MODULE_${itk-module}_IN_ALL ${ITK_BUILD_ALL_MODULES})
    endif()
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
  if(Module_${itk-module} OR ITK_MODULE_${itk-module}_IN_ALL)
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

#
# Set up CPack support
#
set(ITK_MODULES_DISABLED_CPACK )
foreach(m ${ITK_MODULES_DISABLED})
  list(APPEND ITK_MODULES_DISABLED_CPACK "/${m}/")
endforeach()
set(CPACK_SOURCE_IGNORE_FILES
  "${ITK_MODULES_DISABLED_CPACK};/\\\\.git")

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Insight Toolkit version ${ITK_VERSION_MAJOR}")
set(CPACK_PACKAGE_VENDOR "ISC")
set(CPACK_PACKAGE_VERSION_MAJOR "${ITK_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${ITK_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${ITK_VERSION_PATCH}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.txt")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")

include(CPack)

# Report what will be built.
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    if(Module_${itk-module})
      set(_reason ", requested by Module_${itk-module}")
    elseif(ITK_MODULE_${itk-module}_IN_ALL)
      set(_reason ", requested by ITK_BUILD_ALL_MODULES")
    else()
      set(_reason ", needed by [${ITK_MODULE_${itk-module}_NEEDED_BY}]")
    endif()
    message(STATUS "Enabled ${itk-module}${_reason}.")
  endif()
endforeach()

# Hide options for modules that will build anyway.
foreach(itk-module ${ITK_MODULES_ALL})
  if(NOT ${itk-module}_IS_TEST)
    if(ITK_MODULE_${itk-module}_IN_ALL OR ITK_MODULE_${itk-module}_NEEDED_BY)
      set_property(CACHE Module_${itk-module} PROPERTY TYPE INTERNAL)
    else()
      set_property(CACHE Module_${itk-module} PROPERTY TYPE BOOL)
    endif()
  endif()
endforeach()

#-----------------------------------------------------------------------------
# Construct an in-build-order list of "CDash subproject" modules from the
# list of enabled modules:

set(ITK_CDASH_SUBPROJECT_MODULES)

if(ITK_GENERATE_PROJECT_XML OR ITK_GENERATE_SUBPROJECTS_CMAKE)
  # Since a "CDash subproject" for ITK logically contains both a non-test
  # module and its corresponding test module, the subproject order must take
  # both modules into account. The subprojects in this list are named after
  # the non-test modules in the enabled modules list. But the ordering of
  # these subprojects use the dependencies of the modules *and* their test
  # modules
  foreach(module ${ITK_MODULES_ENABLED})
    if(${module}_TESTED_BY)
      # module that HAS a test module: skip for now... will be added as a
      # subproject later, in the slot when its corresponding "-test" module
      # is encountered
    elseif(${module}_TESTS_FOR)
      # this is a test module... *now* list the module which it tests as
      # the subproject name:
      list(APPEND ITK_CDASH_SUBPROJECT_MODULES ${${module}_TESTS_FOR})
    else()
      # a module that is not a test module, and has no tested by...
      # is just a module-only subproject with no test module:
      list(APPEND ITK_CDASH_SUBPROJECT_MODULES ${module})
    endif()
  endforeach()
endif()

#-----------------------------------------------------------------------------
# Write a Project.xml file to send the description of the submodules and
# their dependencies up to CDash:

if(ITK_GENERATE_PROJECT_XML)
  set(filename "${ITK_BINARY_DIR}/${main_project_name}.Project.xml")

  set(xml "<?xml version='1.0' encoding='UTF-8'?>\n")
  set(xml "${xml}<Project name='${main_project_name}'>\n")
  foreach(module ${ITK_CDASH_SUBPROJECT_MODULES})
    if(${module}_IS_TEST)
      message(FATAL_ERROR "unexpected: subproject names should not be test modules module='${module}' tests_for='${${module}_TESTS_FOR}'")
    endif()
    set(xml "${xml}  <SubProject name='${module}'>\n")
    set(deps "")
    set(dep_list ${ITK_MODULE_${module}_DEPENDS})
    if(${module}_TESTED_BY)
      list(APPEND dep_list ${ITK_MODULE_${${module}_TESTED_BY}_DEPENDS})
      if(dep_list)
        list(SORT dep_list)
      endif()
    endif()
    foreach(dep ${dep_list})
      if(NOT ${dep}_IS_TEST AND NOT "${module}" STREQUAL "${dep}")
        set(xml "${xml}    <Dependency name='${dep}'/>\n")
      endif()
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
  foreach(itk-module ${ITK_CDASH_SUBPROJECT_MODULES})
    if(${itk-module}_IS_TEST)
      message(FATAL_ERROR "unexpected: subproject names should not be test modules itk-module='${itk-module}' tests_for='${${itk-module}_TESTS_FOR}'")
    endif()
    set(s "${s}  \"${itk-module}\"\n")
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
endmacro()

# Build all modules.
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    init_module_vars()
  endif()
  include("${${itk-module}_SOURCE_DIR}/itk-module-init.cmake" OPTIONAL)
  add_subdirectory("${${itk-module}_SOURCE_DIR}" "${${itk-module}_BINARY_DIR}")
endforeach()

#----------------------------------------------------------------------------
configure_file(CMake/CTestCustom.cmake.in CTestCustom.cmake @ONLY)

#-----------------------------------------------------------------------------

# Create list of available modules and libraries.
set(ITK_CONFIG_MODULES_ENABLED "")
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    list(APPEND ITK_CONFIG_MODULES_ENABLED ${itk-module})
  endif()
endforeach()

# Generate ITKConfig.cmake for the build tree.
set(ITK_CONFIG_CODE "
set(ITK_MODULES_DIR \"${ITK_MODULES_DIR}\")")
set(ITK_USE_FILE "${ITK_SOURCE_DIR}/CMake/UseITK.cmake")
set(ITK_CONFIG_TARGETS_CONDITION " AND NOT ITK_BINARY_DIR")
set(ITK_CONFIG_TARGETS_FILE "${ITK_BINARY_DIR}/ITKTargets.cmake")
set(ITK_CONFIG_MODULE_API_FILE "${ITK_SOURCE_DIR}/CMake/ITKModuleAPI.cmake")
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
endforeach()
set(ITK_CONFIG_CODE "${ITK_CONFIG_CODE}
set(ITK_MODULES_DIR \"\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/Modules\")")
set(ITK_USE_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/UseITK.cmake")
set(ITK_CONFIG_TARGETS_CONDITION "")
set(ITK_CONFIG_TARGETS_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/ITKTargets.cmake")
set(ITK_CONFIG_MODULE_API_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/ITKModuleAPI.cmake")
if(NOT ITK_USE_SYSTEM_FFTW)
  # Location installed with the FFTW ExternalProject.
  set(FFTW_LIBDIR "\${ITK_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
  set(FFTW_INCLUDE_PATH "\${ITK_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Algorithms")
endif()
configure_file(CMake/ITKConfig.cmake.in CMakeFiles/ITKConfig.cmake @ONLY)

#----------------------------------------------------------------------------
# Configure maintenance scripts
configure_file(Utilities/Maintenance/doSingleKWStyleUncrustifyFix.sh.in
  Utilities/Maintenance/doSingleKWStyleUncrustifyFix.sh @ONLY)

#-----------------------------------------------------------------------------
configure_file(CMake/ITKConfigVersion.cmake.in ITKConfigVersion.cmake @ONLY)

install(FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKConfig.cmake
              ${ITK_BINARY_DIR}/ITKConfigVersion.cmake
              CMake/ITKModuleAPI.cmake
              CMake/UseITK.cmake
              CMake/itkImageIOFactoryRegisterManager.h.in
              CMake/itkTransformIOFactoryRegisterManager.h.in
  DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
  COMPONENT Development)
get_property(ITKTargets_MODULES GLOBAL PROPERTY ITKTargets_MODULES)
if(ITKTargets_MODULES)
  install(EXPORT ITKTargets DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
          COMPONENT Development)
else()
  set(CMAKE_CONFIGURABLE_FILE_CONTENT "# No targets!")
  configure_file(${CMAKE_ROOT}/Modules/CMakeConfigurableFile.in
                 ${ITK_BINARY_DIR}/CMakeFiles/ITKTargets.cmake @ONLY)
  install(FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKTargets.cmake
          DESTINATION ${ITK_INSTALL_PACKAGE_DIR} COMPONENT Development)
endif()

install(FILES "LICENSE" "NOTICE" "README.txt" DESTINATION ${ITK_INSTALL_DOC_DIR} COMPONENT Runtime)

if(BUILD_TESTING)
  add_subdirectory(Utilities/InstallTest)
endif()

add_subdirectory(Modules/Remote)

#-----------------------------------------------------------------------------
# The subdirectories added below this line should use only the public
# interface with find_package(ITK).  Set ITK_DIR to use this ITK build.
set(ITK_DIR "${ITK_BINARY_DIR}")

include(Wrapping/itkWrapSetup.cmake)
if(ITK_WRAPPING)
  if(ITK_USE_64BITS_IDS AND WIN32)
    message(FATAL_ERROR "WRAPPING with ITK_USE_64BITS_IDS is not supported on Windows.
Please turn of ITK_USE_64BITS_IDS.")
  endif()
  add_subdirectory(Wrapping)
endif()

if(BUILD_EXAMPLES)
  add_subdirectory(Examples)
endif()

#----------------------------------------------------------------------
# Provide an option for generating documentation.
add_subdirectory(Utilities/Doxygen)

# Create target to download data from the ITKData group.  This must come after
# all tests have been added that reference the group, so we put it last.
ExternalData_Add_Target(ITKData)
