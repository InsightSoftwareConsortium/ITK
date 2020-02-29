cmake_minimum_required(VERSION 3.10.2)

project(MGHIO
  VERSION 5.1.0       #Version should track with ITK
  LANGUAGES CXX C)

set(MGHIO_LIBRARIES MGHIO)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
