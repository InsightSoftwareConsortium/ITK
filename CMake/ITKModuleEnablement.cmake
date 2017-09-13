# This script sorts out the module dependencies, provides user options for customizing
# the list of modules to be built, and enables modules accordingly.

# Load the module DAG.
set(ITK_MODULES_ALL)
file(GLOB_RECURSE meta RELATIVE "${ITK_SOURCE_DIR}"
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
    message(AUTHOR_WARNING "No such module \"${itk-module}\" needed by \"${_needed_by}\"")
  elseif(check_started_${itk-module} AND NOT check_finished_${itk-module})
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
# Provide an option to build the default set of ITK modules. Only a small
# set of modules are excluded and they have the "EXCLUDE_FROM_DEFAULT" tags in
# their module definition file (itk-module.cmake).
#
# However, if you choose to customize which modules will be built, ITK also
# allows you to manually enable modules by using either individual Module_*
# options or ITKGroup_* options.
option(ITK_BUILD_DEFAULT_MODULES "Build the default ITK modules." ON)

#----------------------------------------------------------------------
# Provide an option to build the tests of dependencies of a module when
# BUILD_TESTING is ON.
option(ITK_BUILD_ALL_MODULES_FOR_TESTS "Build the tests of module dependencies." OFF)
mark_as_advanced(ITK_BUILD_ALL_MODULES_FOR_TESTS)

# To maintain backward compatibility
if(DEFINED ITK_BUILD_ALL_MODULES)
  message(WARNING "ITK_BUILD_ALL_MODULES is deprecated, please remove this entry from the CMake "
                  "cache (edit the CMakeCache.txt file located in the top level of the ITK build "
                  "tree directly or via the CMake GUI), and use ITK_BUILD_DEFAULT_MODULES instead.")
  set(ITK_BUILD_DEFAULT_MODULES ${ITK_BUILD_ALL_MODULES} CACHE BOOL "Build the default ITK modules." FORCE)
endif()
# Provide module selections by groups
include(${ITK_SOURCE_DIR}/CMake/ITKGroups.cmake)

# Provide an option for each module.
foreach(itk-module ${ITK_MODULES_ALL})
  if(NOT ${itk-module}_IS_TEST)
    option(Module_${itk-module} "Request building ${itk-module}" OFF)
    mark_as_advanced(Module_${itk-module})
    if(ITK_MODULE_${itk-module}_EXCLUDE_FROM_DEFAULT)
      set(ITK_MODULE_${itk-module}_IN_DEFAULT 0)
    else()
      set(ITK_MODULE_${itk-module}_IN_DEFAULT ${ITK_BUILD_DEFAULT_MODULES})
    endif()
  endif()
endforeach()

# Follow dependencies.
macro(itk_module_enable itk-module _needed_by)
  if(NOT ITK_MODULE_${itk-module}_DECLARED)
    message(FATAL_ERROR "No such module \"${itk-module}\" needed by \"${_needed_by}\"")
  endif()
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
    if(${itk-module}_TESTED_BY AND (ITK_BUILD_DEFAULT_MODULES OR ITK_BUILD_ALL_MODULES_FOR_TESTS OR Module_${itk-module}))
      itk_module_enable(${${itk-module}_TESTED_BY} "")
    endif()
  endif()
endmacro()

foreach(itk-module ${ITK_MODULES_ALL})
  if(Module_${itk-module} OR ITK_MODULE_${itk-module}_IN_DEFAULT)
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
set(CPACK_PACKAGE_CONTACT "Insight Software Consortium <community@itk.org>")
set(CPACK_PACKAGE_VERSION_MAJOR "${ITK_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${ITK_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${ITK_VERSION_PATCH}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")

include(CPack)

# Report what will be built.
set(_enabled_modules "${ITK_MODULES_ENABLED}")
list(SORT _enabled_modules)
foreach(itk-module ${_enabled_modules})
  if(NOT ${itk-module}_IS_TEST)
    if(Module_${itk-module})
      set(_reason ", requested by Module_${itk-module}")
    elseif(ITK_MODULE_${itk-module}_IN_DEFAULT)
      set(_reason ", requested by ITK_BUILD_DEFAULT_MODULES")
    else()
      set(_reason ", needed by [${ITK_MODULE_${itk-module}_NEEDED_BY}]")
    endif()
    message(STATUS "Enabled ${itk-module}${_reason}.")
  endif()
endforeach()

# Hide options for modules that will build anyway.
foreach(itk-module ${ITK_MODULES_ALL})
  if(NOT ${itk-module}_IS_TEST)
    if(ITK_MODULE_${itk-module}_IN_DEFAULT OR ITK_MODULE_${itk-module}_NEEDED_BY)
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
get_property(CTEST_CUSTOM_MEMCHECK_IGNORE GLOBAL PROPERTY CTEST_CUSTOM_MEMCHECK_IGNORE)
get_property(CTEST_CUSTOM_TESTS_IGNORE GLOBAL PROPERTY CTEST_CUSTOM_TESTS_IGNORE)
configure_file(CMake/CTestCustom.cmake.in CTestCustom.cmake @ONLY)

#-----------------------------------------------------------------------------

# Create list of available modules and libraries.
set(ITK_CONFIG_MODULES_ENABLED "")
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(NOT ${itk-module}_IS_TEST)
    list(APPEND ITK_CONFIG_MODULES_ENABLED ${itk-module})
  endif()
endforeach()
