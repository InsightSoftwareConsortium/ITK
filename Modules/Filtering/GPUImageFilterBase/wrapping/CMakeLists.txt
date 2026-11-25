if(ITK_USE_GPU)
  itk_wrap_module(ITKGPUImageFilterBase)
  itk_auto_load_and_end_wrap_submodules()
endif()
