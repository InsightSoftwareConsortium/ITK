cmake_minimum_required(VERSION 3.10.2)
project(MorphologicalContourInterpolation)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 4.9 REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()

itk_module_examples()
