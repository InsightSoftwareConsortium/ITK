cmake_minimum_required(VERSION 3.16.3)
project(ITKBridgeNumPy)

set(
  ITKBridgeNumPy_GENEX_SYSTEM_INCLUDE_DIRS
  "$<BUILD_INTERFACE:${Python3_INCLUDE_DIRS}>"
)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
