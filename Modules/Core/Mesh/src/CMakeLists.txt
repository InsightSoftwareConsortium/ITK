set(ITKMesh_SRCS
  itkMeshRegion.cxx
  itkSimplexMeshGeometry.cxx
  itkTriangleMeshCurvatureCalculator.cxx
        )

itk_module_add_library(ITKMesh ${ITKMesh_SRCS})
