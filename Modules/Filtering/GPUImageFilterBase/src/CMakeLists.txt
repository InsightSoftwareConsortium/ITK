if (ITK_USE_GPU)
  set(ITKGPUImageFilterBase_SRC
    )

  set(ITKGPUImageFilterBase_Kernels
    GPUNeighborhoodOperatorImageFilter.cl
    )

  write_gpu_kernels("${ITKGPUImageFilterBase_Kernels}" ITKGPUImageFilterBase_SRC)

  add_library(ITKGPUImageFilterBase ${ITKGPUImageFilterBase_SRC})
  target_link_libraries(ITKGPUImageFilterBase ${ITKGPUCommon_LIBRARIES} ${ITKCommon_LIBRARIES} ${OPENCL_LIBRARIES} )
  itk_module_target(ITKGPUImageFilterBase)
endif()
