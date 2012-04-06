if (ITK_USE_GPU)
  set(ITKGPUPDEDeformable_SRC
   )

  set(ITKGPUPDEDeformable_Kernels
    GPUDemonsRegistrationFunction.cl
    GPUPDEDeformableRegistrationFilter.cl
    )

  write_gpu_kernels("${ITKGPUPDEDeformable_Kernels}" ITKGPUPDEDeformable_SRC)

  add_library(ITKGPUPDEDeformable ${ITKGPUPDEDeformable_SRC})
  target_link_libraries(ITKGPUPDEDeformable ${ITKGPUFiniteDifference} ${ITKGPUCommon_LIBRARIES} ${ITKCommon_LIBRARIES} ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUPDEDeformable)
endif()
