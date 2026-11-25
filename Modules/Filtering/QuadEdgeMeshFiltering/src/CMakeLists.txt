set(
  ITKQuadEdgeMeshFiltering_SRCS
  itkBorderQuadEdgeMeshFilter.cxx
  itkLaplacianDeformationQuadEdgeMeshFilter.cxx
  itkNormalQuadEdgeMeshFilter.cxx
)

itk_module_add_library(ITKQuadEdgeMeshFiltering ${ITKQuadEdgeMeshFiltering_SRCS})
