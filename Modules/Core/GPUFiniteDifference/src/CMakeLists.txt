if (ITK_USE_GPU)
  set(ITKGPUFiniteDifference_SRCS
    )

  set(ITKGPUFiniteDifference_Kernels
    GPUDenseFiniteDifferenceImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUFiniteDifference_Kernels}" ITKGPUFiniteDifference_SRCS)

  itk_module_add_library(ITKGPUFiniteDifference ${ITKGPUFiniteDifference_SRCS})
  target_link_libraries(ITKGPUFiniteDifference LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
