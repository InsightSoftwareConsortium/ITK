project(ITKGPURegistrationCommon)

if (ITK_USE_GPU)
  set(ITKGPURegistrationCommon_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPURegistrationCommon_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
