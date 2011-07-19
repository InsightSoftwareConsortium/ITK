WRAP_LIBRARY(ITKQuadEdgeMesh)

set(WRAPPER_LIBRARY_DEPENDS ITKMesh)

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

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
