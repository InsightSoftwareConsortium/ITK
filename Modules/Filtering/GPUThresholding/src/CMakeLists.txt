if (ITK_USE_GPU)
  set(ITKGPUThresholding_SRCS
   )

  set(ITKGPUThresholding_Kernels
    GPUBinaryThresholdImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUThresholding_Kernels}" ITKGPUThresholding_SRCS)

  itk_module_add_library(ITKGPUThresholding ${ITKGPUThresholding_SRCS})
  target_link_libraries(ITKGPUThresholding LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
