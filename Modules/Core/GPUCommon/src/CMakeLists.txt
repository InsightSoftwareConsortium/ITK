if (ITK_USE_GPU)
  set(ITKGPUCommon_SRCS
    itkGPUContextManager.cxx
    itkGPUDataManager.cxx
    itkGPUKernelManager.cxx
    itkOpenCLUtil.cxx
    )

  set(ITKGPUCommon_Kernels
    GPUImageOps.cl
    GPUReduction.cl
    )

  write_gpu_kernels("${ITKGPUCommon_Kernels}" ITKGPUCommon_SRCS)

  itk_module_add_library(ITKGPUCommon ${ITKGPUCommon_SRCS})
  target_link_libraries(ITKGPUCommon LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
