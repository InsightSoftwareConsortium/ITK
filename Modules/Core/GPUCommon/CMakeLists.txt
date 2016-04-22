project(ITKGPUCommon)

# Check for Blocks (see comments in .cxx test file)
try_compile(ITK_COMPILER_HAS_BLOCKS
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasBlocks.cxx)

if(ITK_USE_GPU AND APPLE AND NOT ITK_COMPILER_HAS_BLOCKS)
 message(FATAL_ERROR "Your compiler does not support Blocks (C language extension). This is needed for ITK_USE_GPU=ON")
endif()

if (ITK_USE_GPU)
  set(ITKGPUCommon_LIBRARIES ITKGPUCommon)
  set(ITKGPUCommon_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUCommon_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
