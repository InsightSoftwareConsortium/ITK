project(ITKGPUSmoothing)

if(ITK_USE_GPU)
  set(ITKGPUSmoothing_LIBRARIES ITKGPUSmoothing)
endif()

itk_module_impl()
