project(ITKGPUPDEDeformableRegistration)

if(ITK_USE_GPU)
  set(ITKGPUPDEDeformableRegistration_LIBRARIES ITKGPUPDEDeformableRegistration)
endif()

itk_module_impl()
