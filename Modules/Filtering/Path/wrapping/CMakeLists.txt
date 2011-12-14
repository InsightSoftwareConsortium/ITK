itk_wrap_module(ITKPath)

set(WRAPPER_LIBRARY_GROUPS
  itkPathBase
  itkParametricPath
  itkPolyLineParametricPath
)
itk_auto_load_submodules()

itk_end_wrap_module()
