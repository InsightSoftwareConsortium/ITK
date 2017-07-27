cmake_minimum_required(VERSION 2.8.9)
project(ModuleTemplate)

set(ModuleTemplate_LIBRARIES ModuleTemplate)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
