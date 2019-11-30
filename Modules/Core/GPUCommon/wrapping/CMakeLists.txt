if(ITK_USE_GPU OR NOT ITK_SOURCE_DIR)

itk_wrap_module(ITKGPUCommon)
set(WRAPPER_SUBMODULE_ORDER
  itkGPUContextManager
  itkGPUKernelManager
  itkGPUDataManager
  itkGPUImage
  itkGPUImageToImageFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()

endif()
