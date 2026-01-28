cmake_minimum_required(VERSION 3.16.3 FATAL_ERROR)
project(ITKGPUAnisotropicSmoothing)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  set(ITK_USE_GPU ON CACHE BOOL "Enable OpenCL GPU support." FORCE)
  include(itkOpenCL)
endif()

if(ITK_USE_GPU)
  set(ITKGPUAnisotropicSmoothing_LIBRARIES ITKGPUAnisotropicSmoothing)
endif()

if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
