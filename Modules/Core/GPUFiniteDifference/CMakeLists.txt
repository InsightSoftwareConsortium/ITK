project(ITKGPUFiniteDifference)

if (ITK_USE_GPU)
  set(ITKGPUFiniteDifference_LIBRARIES ITKGPUFiniteDifference)
  set(ITKGPUFiniteDifference_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUFiniteDifference_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
