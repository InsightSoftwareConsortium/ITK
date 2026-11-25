if(ITK_USE_GPU)
  set(ITKGPUSmoothing_SRCS)

  set(ITKGPUSmoothing_Kernels GPUMeanImageFilter.cl)

  write_gpu_kernels("${ITKGPUSmoothing_Kernels}" ITKGPUSmoothing_SRCS)

  itk_module_add_library(ITKGPUSmoothing ${ITKGPUSmoothing_SRCS})
endif()
