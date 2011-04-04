set(ITK-Mesh_SRC
itkMeshRegion.cxx
itkSimplexMeshGeometry.cxx
)

add_library(ITK-Mesh ${ITK-Mesh_SRC})
target_link_libraries(ITK-Mesh  ${ITK-Transform_LIBRARIES})
itk_module_target(ITK-Mesh)
