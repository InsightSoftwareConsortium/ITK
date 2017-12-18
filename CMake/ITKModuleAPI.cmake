

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
    list(APPEND ${ns}_RUNTIME_LIBRARY_DIRS ${${mod}_RUNTIME_LIBRARY_DIRS})
    if(${mod}_FACTORY_NAMES)
      foreach(_factory_format ${${mod}_FACTORY_NAMES})
        # Split <factory_name>::<format>
        string(REGEX REPLACE "^(.*)::(.*)$" "\\1" _factory_name "${_factory_format}")
        string(REGEX REPLACE "^(.*)::(.*)$" "\\2" _format "${_factory_format}")
        list(APPEND ${ns}_${_factory_name} ${_format})
        list(APPEND ${ns}_FACTORY_NAMES ${mod}::${_factory_format})
        list(APPEND ${ns}_FACTORY_LIST ${_factory_name})
        # Configure factory Meta-modules
        set(_meta_module ITK${_factory_name})
        set(${_meta_module}_LOADED TRUE)
        list(APPEND ${_meta_module}_TRANSITIVE_DEPENDS ${mod})
        list(REMOVE_DUPLICATES ${_meta_module}_TRANSITIVE_DEPENDS)
        list(SORT ${_meta_module}_TRANSITIVE_DEPENDS) # Sort to ensure a deterministic order
        #
      endforeach()
    endif()
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
#  <module>_RUNTIME_LIBRARY_DIRS = Runtime linker search path
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
#  <namespace>_RUNTIME_LIBRARY_DIRS = Runtime linker search path
#  <namespace>_FACTORY_NAMES = List of formats to register
#  <namespace>_FACTORY_LIST = List of factories to register
#  <namespace>_<factory_list> = List of formats in each factory
# Do not name a module as the namespace.
#
# When a module name corresponds to a factory, a meta-module with the same name
# is created. It is marked as LOADED and it lists its dependencies (all the
# modules invoking the corresponding factory) in TRANSITIVE_DEPENDS modules.
#
# `itk_module_config()` can be called with a <namespace> that is different from
# the default one (ITK) to avoid overwritting the regular ITK_* variables.
#
# This first call of `itk_module_config()` will create the meta-modules. This
# is done when calling `find_package(ITK)`.
#
# Caveat: The logic configuring the itk<factory_type>FactoryRegisterManager.h
# headers is only available in UseITK. This means that automatic loading of
# the IO factories will not happen by only relying on find_package(ITK
# COMPONENTS ITKCore) and itk_module_config(). This will be improved in the
# future. For more details, read documentation in CMake/UseITK.cmake.
macro(itk_module_config ns)
  set(${ns}_LIBRARIES "")
  set(${ns}_INCLUDE_DIRS "")
  set(${ns}_LIBRARY_DIRS "")
  set(${ns}_RUNTIME_LIBRARY_DIRS "")
  set(${ns}_FACTORY_NAMES "")
  foreach(_factory_name ${${ns}_FACTORY_LIST})
    unset(${ns}_${_factory_name})
  endforeach()
  set(${ns}_FACTORY_LIST "")
  set(_${ns}_USED_MODULES "")
  foreach(mod ${ARGN})
    _itk_module_config_recurse("${ns}" "${mod}")
  endforeach()
  foreach(mod ${_${ns}_USED_MODULES})
    unset(_${ns}_${mod}_USED)
  endforeach()
  unset(_${ns}_USED_MODULES)

  foreach(v ${ns}_LIBRARIES ${ns}_INCLUDE_DIRS ${ns}_LIBRARY_DIRS
            ${ns}_RUNTIME_LIBRARY_DIRS ${ns}_FACTORY_NAMES ${ns}_FACTORY_LIST)
    if(${v})
      list(REMOVE_DUPLICATES ${v})
    endif()
  endforeach()
  foreach(_factory ${${ns}_FACTORY_LIST})
    list(SORT ${ns}_${_factory}) # Sort to ensure a deterministic order
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
