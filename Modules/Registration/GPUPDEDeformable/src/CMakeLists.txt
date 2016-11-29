if (ITK_USE_GPU)
  set(ITKGPUPDEDeformableRegistration_SRCS
   )

  set(ITKGPUPDEDeformableRegistration_Kernels
    GPUDemonsRegistrationFunction.cl
    GPUPDEDeformableRegistrationFilter.cl
    )

  write_gpu_kernels("${ITKGPUPDEDeformableRegistration_Kernels}" ITKGPUPDEDeformableRegistration_SRCS)

  itk_module_add_library(ITKGPUPDEDeformableRegistration ${ITKGPUPDEDeformableRegistration_SRCS})
  target_link_libraries(ITKGPUPDEDeformableRegistration LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
