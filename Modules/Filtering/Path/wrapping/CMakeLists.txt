itk_wrap_module(ITKPath)

set(WRAPPER_SUBMODULE_ORDER
  itkPathBase
  itkPolyLineParametricPath
  itkPathToImageFilter
  itkParametricPath
  itkImageToPathFilter
)
itk_auto_load_and_end_wrap_submodules()
