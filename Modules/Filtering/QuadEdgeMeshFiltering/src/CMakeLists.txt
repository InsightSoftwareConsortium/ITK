set(ITKQuadEdgeMeshFiltering_SRCS
        itkNormalQuadEdgeMeshFilter.cxx
        itkBorderQuadEdgeMeshFilter.cxx
        itkLaplacianDeformationQuadEdgeMeshFilter.cxx
        )

itk_module_add_library(ITKQuadEdgeMeshFiltering ${ITKQuadEdgeMeshFiltering_SRCS})
