project(ITKGPURegistrationCommon)

if (ITK_USE_GPU)
  set(ITKGPURegistrationCommon_LIBRARIES ITKGPURegistrationCommon)
  set(ITKGPURegistrationCommon_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPURegistrationCommon_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif(ITK_USE_GPU)

itk_module_impl()
