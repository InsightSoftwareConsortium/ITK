cmake_minimum_required(VERSION 3.10.2)

project(AnalyzeObjectMapIO
  VERSION 5.1.0       #Version should track with ITK
  LANGUAGES CXX)

set(AnalyzeObjectMapIO_LIBRARIES AnalyzeObjectMapIO)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()
