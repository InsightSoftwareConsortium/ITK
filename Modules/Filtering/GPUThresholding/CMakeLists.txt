project(ITKGPUThresholding)

if (ITK_USE_GPU)
  set(ITKGPUThresholding_LIBRARIES ITKGPUThresholding)
  set(ITKGPUThresholding_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUThresholding_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
