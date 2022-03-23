if(ITK_USE_GPU OR NOT ITK_SOURCE_DIR)
itk_wrap_module(ITKGPUFiniteDifference)
itk_auto_load_and_end_wrap_submodules()
endif()
