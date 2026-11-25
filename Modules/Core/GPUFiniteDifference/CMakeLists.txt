project(ITKGPUFiniteDifference)

if(ITK_USE_GPU)
  set(ITKGPUFiniteDifference_LIBRARIES ITKGPUFiniteDifference)
endif()

itk_module_impl()
