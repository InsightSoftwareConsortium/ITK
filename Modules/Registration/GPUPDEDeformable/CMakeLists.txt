project(ITKGPUPDEDeformableRegistration)

if (ITK_USE_GPU)
  set(ITKGPUPDEDeformableRegistration_LIBRARIES ITKGPUPDEDeformableRegistration)
  set(ITKGPUPDEDeformableRegistration_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUPDEDeformableRegistration_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
