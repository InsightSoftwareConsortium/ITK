project(ITKGPUImageFilterBase)

if (ITK_USE_GPU)
  set(ITKGPUImageFilterBase_LIBRARIES ITKGPUImageFilterBase)
  set(ITKGPUImageFilterBase_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUImageFilterBase_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
