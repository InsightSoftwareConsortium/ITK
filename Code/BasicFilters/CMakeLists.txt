LINK_LIBRARIES (
ITKCommon
)

INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/BasicFilters
)

SOURCE_FILES(ITKBasicFilters_SRCS
itkWriter
)

ADD_LIBRARY(ITKBasicFilters ITKBasicFilters_SRCS)

# We want to do manual instantiation for this library.
ADD_DEFINITIONS(-DITK_MANUAL_INSTANTIATION)

# Put everything in the "BasicFilters" package (related to CABLE, everything
# will still be put in the ITKBasicFilters library).
CABLE_PACKAGE(BasicFilters ITKBasicFilters)

# Sources of templated classes.
CABLE_SOURCE_FILES(
itkAcosImageFilter
itkAddImageFilter
itkAffineTransformMeshFilter
itkAnisotropicDiffusionImageFilter
itkAsinImageFilter
itkAtan2ImageFilter
itkAtanImageFilter
itkBinaryImageFilter
itkCosImageFilter
itkCurvatureAnisotropicDiffusionImageFilter
itkDanielssonDistanceMapImageFilter
itkDerivativeImageFilter
itkDiscreteGaussianImageFilter
itkExpImageFilter
itkFileIOToImageFilter
itkFirstDerivativeRecursiveGaussianImageFilter
itkGradientAnisotropicDiffusionImageFilter
itkGradientMagnitudeImageFilter
itkGradientRecursiveGaussianImageFilter
itkImageWriter
itkImportImageFilter
itkImportPhysicalImageFilter
itkLog10ImageFilter
itkLogImageFilter
itkMeshSource
itkMeshToMeshFilter
itkMultiplyImageFilter
itkNeighborhoodOperatorImageFilter
itkNonThreadedShrinkImageFilter
itkPlaheImageFilter
itkRandomImageSource
itkRawImageWriter
itkRecursiveGaussianImageFilter
itkRecursiveSeparableImageFilter
itkReflectiveImageRegionIterator
itkSecondDerivativeRecursiveGaussianImageFilter
itkShrinkImageFilter
itkSinImageFilter
itkSqrtImageFilter
itkSubtractImageFilter
itkTanImageFilter
itkTernaryAddImageFilter
itkTernaryImageFilter
itkTernaryMagnitudeImageFilter
itkTernaryMagnitudeSquaredImageFilter
itkThresholdImageFilter
itkUnaryImageFilter
itkVTKImageReader
itkVTKImageWriter
itkVectorAnisotropicDiffusionImageFilter
)

# All our classes are in the "itk" namespace.
CABLE_OPEN_NAMESPACE(itk)
  # This set shortens the instantiation list for all our UnaryImageFilter
  # subclass instantiations.
  CABLE_DEFINE_SET(ifFunc Acos Asin Atan Cos Exp Log10 Log Sin Sqrt Tan)

  # List the instantiations for this package.
  CABLE_INSTANTIATE_CLASS(
    "$(ifFunc)ImageFilter<$ImageType, $ImageType >"
    "ThresholdImageFilter<$ImageType >"
    "ShrinkImageFilter<$PhysicalImageType, $PhysicalImageType >"
    "RecursiveGaussianImageFilter<$PhysicalImageType, $PhysicalImageType, double >"
  )
CABLE_CLOSE_NAMESPACE(itk)

