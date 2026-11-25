project(ITKGPUImageFilterBase)

if(ITK_USE_GPU)
  set(ITKGPUImageFilterBase_LIBRARIES ITKGPUImageFilterBase)
endif()

itk_module_impl()
