cmake_minimum_required(VERSION 3.16.3 FATAL_ERROR)
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
  set(ITKGPUCommon_LIBRARIES ITKGPUCommon ${OpenCL_LIBRARIES})
  if(APPLE)
    list(APPEND ITKGPUCommon_LIBRARIES "-framework OpenCL")
  elseif(WINDOWS)
    list(APPEND ITKGPUCommon_LIBRARIES cfgmgr32 OneCoreUAP)
  endif()

  set(ITKGPUCommon_SYSTEM_INCLUDE_DIRS ${OpenCL_INCLUDE_DIRS})
endif()

if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
