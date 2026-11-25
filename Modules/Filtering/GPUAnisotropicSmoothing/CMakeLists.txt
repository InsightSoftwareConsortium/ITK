project(ITKGPUAnisotropicSmoothing)

if(ITK_USE_GPU)
  set(ITKGPUAnisotropicSmoothing_LIBRARIES ITKGPUAnisotropicSmoothing)
endif()

itk_module_impl()
