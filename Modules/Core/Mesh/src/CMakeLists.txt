set(ITKMesh_SRC
itkMeshRegion.cxx
itkSimplexMeshGeometry.cxx
)

add_library(ITKMesh ${ITKMesh_SRC})
itk_module_link_dependencies()
itk_module_target(ITKMesh)
