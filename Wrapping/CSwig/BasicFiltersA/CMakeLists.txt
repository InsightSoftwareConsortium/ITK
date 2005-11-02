# create the ITKBasicFiltersTcl libraries
SET(WRAP_SOURCES
  wrap_itkAnisotropicDiffusionImageFilter_2D
  wrap_itkAnisotropicDiffusionImageFilter_3D
  wrap_itkBinaryThresholdImageFilter
  wrap_itkBinaryDilateImageFilter
  wrap_itkBinaryErodeImageFilter
  wrap_itkCannyEdgeDetectionImageFilter
  wrap_itkCastImageFilter_2D
  wrap_itkCastImageFilter_3D
  wrap_itkConfidenceConnectedImageFilter
  wrap_itkChangeInformationImageFilter
  wrap_itkConnectedThresholdImageFilter
  wrap_itkCurvatureAnisotropicDiffusionImageFilter
  wrap_itkExtractImageFilter
  wrap_itkFastMarchingImageFilter
  wrap_itkFlipImageFilter
  wrap_itkGradientAnisotropicDiffusionImageFilter
  wrap_itkGradientMagnitudeImageFilter
  wrap_itkGrayscaleDilateImageFilter
  wrap_itkGrayscaleErodeImageFilter
  wrap_itkDanielssonDistanceMapImageFilter
  wrap_itkIsolatedConnectedImageFilter
  wrap_itkImportImageFilter
  wrap_itkLaplacianImageFilter
  wrap_itkMinimumMaximumImageCalculator
  wrap_itkNeighborhoodConnectedImageFilter
  wrap_itkSobelEdgeDetectionImageFilter
  wrap_itkTernaryMagnitudeImageFilter
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
  "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
  "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
  "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
  "${WrapITK_BINARY_DIR}/BasicFiltersA/ITKBasicFiltersA.mdx"
  )

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKBasicFiltersA BasicFiltersA "ITKNumerics;ITKCommonB;ITKCommonA" "" "ITKBasicFilters")
