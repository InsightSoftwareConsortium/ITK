if (ITK_USE_GPU)
  set(ITKGPUPDEDeformableRegistration_SRC
   )

  set(ITKGPUPDEDeformableRegistration_Kernels
    GPUDemonsRegistrationFunction.cl
    GPUPDEDeformableRegistrationFilter.cl
    )

  write_gpu_kernels("${ITKGPUPDEDeformableRegistration_Kernels}" ITKGPUPDEDeformableRegistration_SRC)

  add_library(ITKGPUPDEDeformableRegistration ${ITKGPUPDEDeformableRegistration_SRC})
  itk_module_link_dependencies()
  target_link_libraries(ITKGPUPDEDeformableRegistration LINK_PUBLIC ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUPDEDeformableRegistration)
endif()
