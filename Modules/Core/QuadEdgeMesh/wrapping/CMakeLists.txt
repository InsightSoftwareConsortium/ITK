WRAP_LIBRARY(ITKQuadEdgeMesh)

set(WRAPPER_LIBRARY_GROUPS
  itkQuadEdge
  itkGeometricalQuadEdge
  itkQuadEdgeMeshPoint
  itkQuadEdgeMeshTraits
  itkQuadEdgeCellTraitsInfo
  itkQuadEdgeMeshLineCell
  ITKQuadEdgeMeshBase
  # itkQuadEdgeMesh
  itkQuadEdgeMeshToQuadEdgeMeshFilter
)
AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
