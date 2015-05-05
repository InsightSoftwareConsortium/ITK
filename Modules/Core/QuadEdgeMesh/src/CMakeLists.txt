set(ITKQuadEdgeMesh_SRC
itkQuadEdge.cxx
)

add_library(ITKQuadEdgeMesh ${ITKQuadEdgeMesh_SRC})
target_link_libraries(ITKQuadEdgeMesh ${ITKMesh_LIBRARIES})
itk_module_target(ITKQuadEdgeMesh)
