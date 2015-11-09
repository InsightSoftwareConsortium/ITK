if (ITK_USE_GPU)
  set(ITKGPUCommon_SRC
    itkGPUContextManager.cxx
    itkGPUDataManager.cxx
    itkGPUKernelManager.cxx
    itkOpenCLUtil.cxx
    )

  set(ITKGPUCommon_Kernels
    GPUImageOps.cl
    GPUReduction.cl
    )

  write_gpu_kernels("${ITKGPUCommon_Kernels}" ITKGPUCommon_SRC)

  add_library(ITKGPUCommon ${ITKGPUCommon_SRC})
  itk_module_link_dependencies()
  target_link_libraries(ITKGPUCommon LINK_PUBLIC ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUCommon)
endif()
