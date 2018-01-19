cmake_minimum_required(VERSION 3.9.5)
project(MorphologicalContourInterpolation)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 4.9 REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()

itk_module_examples()
