# create the ITKBasicFiltersTcl libraries
SET(WRAP_SOURCES
  wrap_itkAnisotropicDiffusionImageFilter
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
  wrap_itkExpImageFilter
  wrap_itkExpNegativeImageFilter
  wrap_itkExtractImageFilter
  wrap_itkFastMarchingImageFilter
  wrap_itkFlipImageFilter
  wrap_itkGradientAnisotropicDiffusionImageFilter
  wrap_itkGradientRecursiveGaussianImageFilter
  wrap_itkGradientMagnitudeImageFilter
  wrap_itkGradientMagnitudeRecursiveGaussianImageFilter
  wrap_itkDanielssonDistanceMapImageFilter
  wrap_itkIsolatedConnectedImageFilter
  wrap_itkLaplacianImageFilter
  wrap_itkMeanImageFilter
  wrap_itkMedianImageFilter
  wrap_itkMinimumMaximumImageCalculator
  wrap_itkNaryAddImageFilter
  wrap_itkNeighborhoodConnectedImageFilter
  wrap_itkNormalizeImageFilter
  wrap_itkPermuteAxesImageFilter
  wrap_itkRandomImageSource
  wrap_itkRecursiveGaussianImageFilter
  wrap_itkRecursiveSeparableImageFilter
  wrap_itkRegionOfInterestImageFilter
  wrap_itkResampleImageFilter
  wrap_itkRescaleIntensityImageFilter
  wrap_itkShiftScaleImageFilter
  wrap_itkSigmoidImageFilter
  wrap_itkStatisticsImageFilter
  wrap_itkSobelEdgeDetectionImageFilter
  wrap_itkSmoothingRecursiveGaussianImageFilter
  wrap_itkTernaryMagnitudeImageFilter
  wrap_itkThresholdImageFilter
  wrap_itkVTKImageExport
  wrap_itkVTKImageImport
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
  "${WrapITK_BINARY_DIR}/Numerics/ITKNumerics.mdx"
  "${WrapITK_BINARY_DIR}/Common/ITKCommon.mdx"
  "${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx"
  )

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKBasicFilters BasicFilters "_ITKNumericsPython;_ITKCommonPython" "" "")
