cmake_minimum_required(VERSION 3.16.3)
project(IOMeshMZ3)

set(IOMeshMZ3_LIBRARIES IOMeshMZ3)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

itk_module_examples()
