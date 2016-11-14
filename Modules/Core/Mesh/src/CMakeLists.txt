set(ITKMesh_SRCS
  itkMeshRegion.cxx
  itkSimplexMeshGeometry.cxx
  )

itk_module_add_library(ITKMesh ${ITKMesh_SRCS})
