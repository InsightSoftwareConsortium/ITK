itk_wrap_module(ITKStatistics)

set(WRAPPER_SUBMODULE_ORDER
  itkSample
  itkHistogram
  itkListSample
  itkMembershipFunctionBase
)
itk_auto_load_submodules()
itk_end_wrap_module()
