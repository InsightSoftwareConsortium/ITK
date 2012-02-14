if (ITK_USE_GPU)
  set(ITKGPUSmoothing_SRC
    )

  set(ITKGPUSmoothing_Kernels
    GPUMeanImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUSmoothing_Kernels}" ITKGPUSmoothing_SRC)

  add_library(ITKGPUSmoothing ${ITKGPUSmoothing_SRC})
  target_link_libraries(ITKGPUSmoothing
  ${ITKGPUImageFilterBase_LIBRARIES} ${ITKGPUCommon_LIBRARIES} ${ITKCommon_LIBRARIES} ${OPENCL_LIBRARIES} )
  itk_module_target(ITKGPUSmoothing)
endif()
