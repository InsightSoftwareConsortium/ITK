cmake_minimum_required(VERSION 2.8.9)
project(MorphologicalContourInterpolation)

set(
  MCI_Algorithm
  "B"
  CACHE STRING
  "Which algorithm should be used for testing
the module MorphologicalContourInterpolation.

B = repeated dilations with ball structuring element
C = repeated dilations with cross structuring element
T = distance transform (not geodesic!)"
)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 4.9 REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  set(MCI_TestWithRLEImage 0 CACHE BOOL "Whether to test with RLEImage")
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
