LIBRARY(ITKBasicFilters)

LINK_LIBRARIES (
ITKCommon
)

INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/BasicFilters
)

SOURCE_FILES(
itkWriter
)

# We want to do manual instantiation for this library.
UNIX_DEFINES(-DITK_MANUAL_INSTANTIATION)
WIN32_DEFINES(-DITK_MANUAL_INSTANTIATION)

# Put everything in the "BasicFilters" package (related to CABLE, everything
# will still be put in the ITKBasicFilters library).
CABLE_PACKAGE(BasicFilters)

# Sources of templated classes.
CABLE_SOURCE_FILES(
itkAcosImageFilter
itkAsinImageFilter
itkAtanImageFilter
itkCosImageFilter
itkExpImageFilter
itkLog10ImageFilter
itkLogImageFilter
itkSinImageFilter
itkSqrtImageFilter
itkTanImageFilter
itkPlaheImageFilter
)

# All our classes are in the "itk" namespace.
CABLE_OPEN_NAMESPACE(itk)
  # This set shortens the instantiation list for all our UnaryImageFilter
  # subclass instantiations.
  CABLE_DEFINE_SET(ifFunc Acos Asin Atan Cos Exp Log10 Log Sin Sqrt Tan)

  # List the instantiations for this package.
  CABLE_INSTANTIATE_CLASS(
    "$(ifFunc)ImageFilter<$ImageType, $ImageType >"
    "PlaheImageFilter<$PixelType, $Dimension>"
  )
CABLE_CLOSE_NAMESPACE(itk)
