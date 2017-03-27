itk_wrap_module(ITKPath)

set(WRAPPER_SUBMODULE_ORDER
  itkPathBase
  itkPolyLineParametricPath
  itkPathToImageFilter
  itkParametricPath
)
itk_auto_load_submodules()

itk_end_wrap_module()
