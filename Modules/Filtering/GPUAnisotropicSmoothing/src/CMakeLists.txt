if (ITK_USE_GPU)
  set(ITKGPUAnisotropicSmoothing_SRCS
    )

  set(ITKGPUAnisotropicSmoothing_Kernels
    GPUGradientNDAnisotropicDiffusionFunction.cl
    GPUScalarAnisotropicDiffusionFunction.cl
    )

  write_gpu_kernels("${ITKGPUAnisotropicSmoothing_Kernels}" ITKGPUAnisotropicSmoothing_SRCS)

  itk_module_add_library(ITKGPUAnisotropicSmoothing ${ITKGPUAnisotropicSmoothing_SRCS})
  target_link_libraries(ITKGPUAnisotropicSmoothing LINK_PUBLIC ${OPENCL_LIBRARIES})
endif()
