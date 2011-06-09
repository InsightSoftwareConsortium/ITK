WRAP_LIBRARY(Mesh)

set(WRAPPER_LIBRARY_DEPENDS Base)

set(WRAPPER_LIBRARY_GROUPS
  itkDefaultDynamicMeshTraits
  itkDefaultStaticMeshTraits
  itkQuadEdge
  itkGeometricalQuadEdge
  itkQuadEdgeMeshPoint
  itkQuadEdgeMeshTraits
  itkQuadEdgeCellTraitsInfo
  itkCellInterface
  itkQuadEdgeMeshBase
  itkQuadEdgeMeshLineCell
  itkPointSet
  itkMesh
  itkQuadEdgeMesh
  itkMeshSource
  itkImageToMeshFilter
  itkVertexCell
  itkLineCell
  itkMeshToMeshFilter
  itkQuadEdgeMeshToQuadEdgeMeshFilter
  itkDiscretePrincipalCurvaturesQuadEdgeMeshFilter
)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
