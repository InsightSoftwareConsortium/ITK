itk_wrap_module(ITKMesh)

set(WRAPPER_LIBRARY_GROUPS
  itkMeshBase
  itkMeshSource
  itkImageToMeshFilter
  itkMeshToMeshFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()
