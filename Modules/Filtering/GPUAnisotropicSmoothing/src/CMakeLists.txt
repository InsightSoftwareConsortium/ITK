if (ITK_USE_GPU)
  set(ITKGPUAnisotropicSmoothing_SRC
    )

  set(ITKGPUAnisotropicSmoothing_Kernels
    GPUGradientNDAnisotropicDiffusionFunction.cl
    GPUScalarAnisotropicDiffusionFunction.cl
    )

  write_gpu_kernels("${ITKGPUAnisotropicSmoothing_Kernels}" ITKGPUAnisotropicSmoothing_SRC)

  add_library(ITKGPUAnisotropicSmoothing ${ITKGPUAnisotropicSmoothing_SRC})
  itk_module_link_dependencies()
  target_link_libraries(ITKGPUAnisotropicSmoothing LINK_PUBLIC ${OPENCL_LIBRARIES})
  itk_module_target(ITKGPUAnisotropicSmoothing)
endif()
