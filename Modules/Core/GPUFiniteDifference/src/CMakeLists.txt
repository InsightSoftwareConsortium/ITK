if (ITK_USE_GPU)
  set(ITKGPUFiniteDifference_SRC
    )

  set(ITKGPUFiniteDifference_Kernels
    GPUDenseFiniteDifferenceImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUFiniteDifference_Kernels}" ITKGPUFiniteDifference_SRC)

  add_library(ITKGPUFiniteDifference ${ITKGPUFiniteDifference_SRC})
  itk_module_link_dependencies()
  target_link_libraries(ITKGPUFiniteDifference LINK_PUBLIC ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUFiniteDifference)
endif()
