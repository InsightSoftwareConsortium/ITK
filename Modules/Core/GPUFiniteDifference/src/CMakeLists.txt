set(ITKGPUFiniteDifference_SRCS
  itkGPUFiniteDifferenceFilterTypeEnum.cxx
)

if (ITK_USE_GPU)
  set(ITKGPUFiniteDifference_Kernels
    GPUDenseFiniteDifferenceImageFilter.cl
    )
  write_gpu_kernels("${ITKGPUFiniteDifference_Kernels}" ITKGPUFiniteDifference_SRCS)
endif()

itk_module_add_library(ITKGPUFiniteDifference ${ITKGPUFiniteDifference_SRCS})

if (ITK_USE_GPU)
  target_link_libraries(ITKGPUFiniteDifference LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
