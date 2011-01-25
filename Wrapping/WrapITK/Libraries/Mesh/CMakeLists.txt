WRAP_LIBRARY(Mesh)

set(WRAPPER_LIBRARY_DEPENDS Base)

set(WRAPPER_LIBRARY_GROUPS
  itkDefaultDynamicMeshTraits
  itkDefaultStaticMeshTraits
  itkQuadEdge
  itkGeometricalQuadEdge
  itkQuadEdgeMeshPoint
  itkQuadEdgeMeshTraits
  itkQuadEdgeMeshCellTraitsInfo
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
)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
