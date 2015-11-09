itk_wrap_module(ITKCurvatureFlow)

set(WRAPPER_SUBMODULE_ORDER
  itkCurvatureFlowImageFilter
  itkMinMaxCurvatureFlowImageFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()
