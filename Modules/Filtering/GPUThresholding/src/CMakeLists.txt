if (ITK_USE_GPU)
  set(ITKGPUThresholding_SRC
   )

  set(ITKGPUThresholding_Kernels
    GPUBinaryThresholdImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUThresholding_Kernels}" ITKGPUThresholding_SRC)

  add_library(ITKGPUThresholding ${ITKGPUThresholding_SRC})
  itk_module_link_dependencies()
  target_link_libraries(ITKGPUThresholding LINK_PUBLIC ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUThresholding)
endif()
