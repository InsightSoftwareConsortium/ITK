project(ITKGPUPDEDeformable)

if (ITK_USE_GPU)
  set(ITKGPUPDEDeformable_LIBRARIES ITKGPUPDEDeformable)
  set(ITKGPUPDEDeformable_SYSTEM_INCLUDE_DIRS ${OPENCL_INCLUDE_DIRS})
  set(ITKGPUPDEDeformable_SYSTEM_LIBRARY_DIRS ${OPENCL_LIBRARIES})
endif()

itk_module_impl()
