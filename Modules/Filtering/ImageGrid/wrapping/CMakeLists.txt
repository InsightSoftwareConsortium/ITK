itk_wrap_module(ITKImageGrid)

set(WRAPPER_SUBMODULE_ORDER
  itkPadImageFilterBase
  itkPadImageFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()
