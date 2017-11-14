#-----------------------------------------------------------------------------
#
# ITKConfig.cmake - ITK CMake configuration file used
# internally for remote modules calling 'find_package()'
# CMake function.
#
# This file is a minimal implementation of ITKConfig.cmake
# that should only be used internally when configuring ITK.
# Some remote modules contain an 'example' folder. The
# example folder is designed to compile both from
# within the source tree of the module and as an independent
# project. This requires to be able to use the CMake function
# 'find_package(ITK)'. When compiled as part of ITK, this
# function needs to be able to find the file ITKConfig.cmake
# which can only be defined at the end of configuring ITK
# as it needs all the ITK CMake variables to be defined.
#
# This file provides a work around and is defined early in
# the ITK configuration process. Since it is run from within
# the ITK source tree, it does not need to define most ITK
# variables as those are already defined. It only ensures
# that the requested components are available.

# Compute set of requested modules.
if(ITK_FIND_COMPONENTS)
  # Specific modules requested by find_package(ITK).
  set(ITK_MODULES_REQUESTED "${ITK_FIND_COMPONENTS}")
else()
  # No specific modules requested.  Use all of them.
  set(ITK_MODULES_REQUESTED "")
  foreach(itk-module ${ITK_MODULES_ENABLED})
    if(NOT ${itk-module}_IS_TEST)
      list(APPEND ITK_MODULES_REQUESTED ${itk-module})
    endif()
  endforeach()
endif()

# Load requested modules and their dependencies into variables:
#  ITK_LIBRARIES       = Libraries to link
#  ITK_INCLUDE_DIRS    = Header file search path
#  ITK_LIBRARY_DIRS    = Library search path (for outside dependencies)
#  ITK_RUNTIME_LIBRARY_DIRS = Runtime linker search path
#  ITK_FACTORY_NAMES = List of <module>::<factory>::<format> to register
#  ITK_FACTORY_LIST  = List of factories
#  ITK_<factory_name> = List of formats for each factory

itk_module_config(ITK ${ITK_MODULES_REQUESTED})
