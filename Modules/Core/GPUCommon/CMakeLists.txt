cmake_minimum_required(VERSION 3.10.2 FATAL_ERROR)
project(ITKGPUCommon)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  set(ITK_USE_GPU ON CACHE BOOL "Enable OpenCL GPU support." FORCE)
  set(ITK_USE_GPU ON)
  include(itkOpenCL)
  # Check for Blocks (see comments in .cxx test file)
  try_compile(ITK_COMPILER_HAS_BLOCKS
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasBlocks.cxx)
else()
  # Check for Blocks (see comments in .cxx test file)
  try_compile(ITK_COMPILER_HAS_BLOCKS
    ${ITK_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasBlocks.cxx)
endif()


if(ITK_USE_GPU AND APPLE AND NOT ITK_COMPILER_HAS_BLOCKS)
 message(FATAL_ERROR "Your compiler does not support Blocks (C language extension). This is needed for ITK_USE_GPU=ON")
endif()

if(ITK_USE_GPU)
  set(ITKGPUCommon_LIBRARIES ITKGPUCommon)
  set(ITKGPUCommon_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUCommon_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
