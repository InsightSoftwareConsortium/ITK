project(ITKGPUSmoothing)

if (ITK_USE_GPU)
  set(ITKGPUSmoothing_LIBRARIES ITKGPUSmoothing)
  set(ITKGPUSmoothing_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUSmoothing_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
