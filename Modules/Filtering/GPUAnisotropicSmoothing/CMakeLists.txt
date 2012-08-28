project(ITKGPUAnisotropicSmoothing)

if (ITK_USE_GPU)
  set(ITKGPUAnisotropicSmoothing_LIBRARIES ITKGPUAnisotropicSmoothing)
  set(ITKGPUAnisotropicSmoothing_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUAnisotropicSmoothing_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
