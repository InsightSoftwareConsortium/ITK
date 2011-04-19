set(ITK-QuadEdgeMesh_SRC
itkQuadEdge.cxx
)

add_library(ITK-QuadEdgeMesh ${ITK-QuadEdgeMesh_SRC})
target_link_libraries(ITK-QuadEdgeMesh  ${ITK-Mesh_LIBRARIES})
itk_module_target(ITK-QuadEdgeMesh)
