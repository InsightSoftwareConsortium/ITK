

#-----------------------------------------------------------------------------
# Private helper macros.

macro(_itk_module_use_recurse mod)
  if(NOT ${dep}_USED)
    set(${mod}_USED 1)
    itk_module_load("${mod}")
    foreach(dep IN LISTS ${mod}_TRANSITIVE_DEPENDS)
      _itk_module_use_recurse(${dep})
    endforeach()
    if(${mod}_INCLUDE_DIRS)
      include_directories(${${mod}_INCLUDE_DIRS})
    endif()
    if(${mod}_LIBRARY_DIRS)
      link_directories(${${mod}_LIBRARY_DIRS})
    endif()
  endif()
endmacro()

macro(_itk_module_config_recurse ns mod)
  if(NOT _${ns}_${mod}_USED)
    set(_${ns}_${mod}_USED 1)
    list(APPEND _${ns}_USED_MODULES ${mod})
    itk_module_load("${mod}")
    list(APPEND ${ns}_LIBRARIES ${${mod}_LIBRARIES})
    list(APPEND ${ns}_INCLUDE_DIRS ${${mod}_INCLUDE_DIRS})
    list(APPEND ${ns}_LIBRARY_DIRS ${${mod}_LIBRARY_DIRS})
    foreach(dep IN LISTS ${mod}_TRANSITIVE_DEPENDS)
      _itk_module_config_recurse("${ns}" "${dep}")
    endforeach()
  endif()
endmacro()

#-----------------------------------------------------------------------------
# Public interface macros.

# itk_module_load(<module>)
#
# Loads variables describing the given module:
#  <module>_LOADED             = True if the module has been loaded
#  <module>_DEPENDS            = List of dependencies on other modules (public link, private link, compile)
#  <module>_PUBLIC_DEPENDS     = List of dependencies on other modules (public link)
#  <module>_TRANSITIVE_DEPENDS = List of dependencies on other modules (public link, compile)
#  <module>_PRIVATE_DEPENDS    = List of dependencies on other modules (private link)
#  <module>_LIBRARIES          = Libraries to link
#  <module>_INCLUDE_DIRS       = Header search path
#  <module>_LIBRARY_DIRS       = Library search path (for outside dependencies)
macro(itk_module_load mod)
  if(NOT ${mod}_LOADED)
    include("${ITK_MODULES_DIR}/${mod}.cmake" OPTIONAL)
    if(NOT ${mod}_LOADED)
      message(FATAL_ERROR "No such module: \"${mod}\"")
    endif()
    # Include the targets file if it has been defined. Targets files other
    # than ITKTargets.cmake are created when modules are built externally. Do not
    # include the targets file inside the module itself -- which occurs in a module's
    # test configuration.
    if(EXISTS "${${mod}_TARGETS_FILE}" AND NOT itk-module STREQUAL mod)
      include("${${mod}_TARGETS_FILE}")
    endif()
  endif()
endmacro()

# itk_module_config(<namespace> [modules...])
#
# Configures variables describing the given modules and their dependencies:
#  <namespace>_LIBRARIES    = Libraries to link
#  <namespace>_INCLUDE_DIRS = Header search path
#  <namespace>_LIBRARY_DIRS = Library search path (for outside dependencies)
# Do not name a module as the namespace.
macro(itk_module_config ns)
  set(${ns}_LIBRARIES "")
  set(${ns}_INCLUDE_DIRS "")
  set(${ns}_LIBRARY_DIRS "")

  set(_${ns}_USED_MODULES "")
  foreach(mod ${ARGN})
    _itk_module_config_recurse("${ns}" "${mod}")
  endforeach()
  foreach(mod ${_${ns}_USED_MODULES})
    unset(_${ns}_${mod}_USED)
  endforeach()
  unset(_${ns}_USED_MODULES)

  foreach(v ${ns}_LIBRARIES ${ns}_INCLUDE_DIRS ${ns}_LIBRARY_DIRS)
    if(${v})
      list(REMOVE_DUPLICATES ${v})
    endif()
  endforeach()
endmacro()

# itk_module_use([modules...])
#
# Adds include directories and link directories for the given modules and
# their dependencies.
macro(itk_module_use)
  foreach(mod ${ARGN})
    _itk_module_use_recurse("${mod}")
  endforeach()
endmacro()
