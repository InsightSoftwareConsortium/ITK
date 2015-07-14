set(ITKQuadEdgeMesh_SRC
itkQuadEdge.cxx
)

add_library(ITKQuadEdgeMesh ${ITKQuadEdgeMesh_SRC})
itk_module_link_dependencies()
itk_module_target(ITKQuadEdgeMesh)
