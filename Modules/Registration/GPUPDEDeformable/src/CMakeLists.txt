if(ITK_USE_GPU)
  set(ITKGPUPDEDeformableRegistration_SRCS)

  set(
    ITKGPUPDEDeformableRegistration_Kernels
    GPUDemonsRegistrationFunction.cl
    GPUPDEDeformableRegistrationFilter.cl
  )

  write_gpu_kernels("${ITKGPUPDEDeformableRegistration_Kernels}" ITKGPUPDEDeformableRegistration_SRCS)

  itk_module_add_library(ITKGPUPDEDeformableRegistration ${ITKGPUPDEDeformableRegistration_SRCS})
endif()
