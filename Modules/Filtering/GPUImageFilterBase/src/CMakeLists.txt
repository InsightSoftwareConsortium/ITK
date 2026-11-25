if(ITK_USE_GPU)
  set(ITKGPUImageFilterBase_SRCS)

  set(
    ITKGPUImageFilterBase_Kernels
    GPUCastImageFilter.cl
    GPUNeighborhoodOperatorImageFilter.cl
  )

  write_gpu_kernels("${ITKGPUImageFilterBase_Kernels}" ITKGPUImageFilterBase_SRCS)

  itk_module_add_library(ITKGPUImageFilterBase ${ITKGPUImageFilterBase_SRCS})
endif()
