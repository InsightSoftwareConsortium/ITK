if (ITK_USE_GPU)
  set(ITKGPUSmoothing_SRC
    )

  set(ITKGPUSmoothing_Kernels
    GPUMeanImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUSmoothing_Kernels}" ITKGPUSmoothing_SRC)

  add_library(ITKGPUSmoothing ${ITKGPUSmoothing_SRC})
  itk_module_link_dependencies()
  target_link_libraries(ITKGPUSmoothing LINK_PUBLIC ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUSmoothing)
endif()
