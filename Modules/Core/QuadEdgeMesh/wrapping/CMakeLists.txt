itk_wrap_module(ITKQuadEdgeMesh)

set(WRAPPER_LIBRARY_GROUPS
  itkQuadEdge
  itkGeometricalQuadEdge
  itkQuadEdgeMeshPoint
  itkQuadEdgeMeshTraits
  itkQuadEdgeCellTraitsInfo
  itkQuadEdgeMeshLineCell
  itkQuadEdgeMeshBase
  # itkQuadEdgeMesh
  itkQuadEdgeMeshToQuadEdgeMeshFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()
