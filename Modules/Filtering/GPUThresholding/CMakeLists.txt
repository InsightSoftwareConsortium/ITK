project(ITKGPUThresholding)

if(ITK_USE_GPU)
  set(ITKGPUThresholding_LIBRARIES ITKGPUThresholding)
  set(ITKGPUThresholding_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
endif()

itk_module_impl()
