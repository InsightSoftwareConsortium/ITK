if (ITK_USE_GPU)
  set(ITKGPUImageFilterBase_SRCS
    )

  set(ITKGPUImageFilterBase_Kernels
    GPUNeighborhoodOperatorImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUImageFilterBase_Kernels}" ITKGPUImageFilterBase_SRCS)

  itk_module_add_library(ITKGPUImageFilterBase ${ITKGPUImageFilterBase_SRCS})
  target_link_libraries(ITKGPUImageFilterBase LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
