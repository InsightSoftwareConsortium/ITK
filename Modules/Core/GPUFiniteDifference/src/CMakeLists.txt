if (ITK_USE_GPU)
  set(ITKGPUFiniteDifference_SRC
    )

  set(ITKGPUFiniteDifference_Kernels
    GPUDenseFiniteDifferenceImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUFiniteDifference_Kernels}" ITKGPUFiniteDifference_SRC)

  add_library(ITKGPUFiniteDifference ${ITKGPUFiniteDifference_SRC})
  target_link_libraries(ITKGPUFiniteDifference
  ${ITKGPUCommon_LIBRARIES} ${ITKCommon_LIBRARIES} ${OPENCL_LIBRARIES} )
  itk_module_target(ITKGPUFiniteDifference)
endif()
