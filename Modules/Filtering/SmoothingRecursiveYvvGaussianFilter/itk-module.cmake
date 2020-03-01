# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in
# SmoothingRecursiveYvvGaussianFilter
# The testing module in SmoothingRecursiveYvvGaussianFilter depends on
# ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
set(ModuleName "SmoothingRecursiveYvvGaussianFilter")
if(ITK_USE_GPU)
  set(${ModuleName}_GPU_DEPENDANCIES "ITKGPUSmoothing")
  set(${ModuleName}_GPU_COMMON "ITKGPUCommon")
endif()
itk_module(
  ${ModuleName}
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKImageFilterBase
    ITKSmoothing
    ${${ModuleName}_GPU_DEPENDANCIES}
    ${${ModuleName}_GPU_COMMON}
  TEST_DEPENDS
    ITKTestKernel #to handle IO in src
    ${${ModuleName}_GPU_COMMON}
    ITKSmoothing
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  # Header only library does not use ENABLE_SHARED
)
