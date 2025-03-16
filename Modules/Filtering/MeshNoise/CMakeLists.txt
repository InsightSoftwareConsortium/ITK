cmake_minimum_required(VERSION 3.22.1...3.29.0 FATAL_ERROR)


project(MeshNoise)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
