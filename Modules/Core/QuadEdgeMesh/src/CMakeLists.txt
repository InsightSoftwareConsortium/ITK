set(ITKQuadEdgeMesh_SRCS
  itkQuadEdge.cxx
  itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.cxx
  )

itk_module_add_library(ITKQuadEdgeMesh ${ITKQuadEdgeMesh_SRCS})
