BEGIN_WRAPPER_LIBRARY(Base)

SET(WRAPPER_LIBRARY_GROUPS
  ITKCommonBase
  itkOffset
  itkArray
  itkFixedArray
  itkPoint
  itkPointSet
  itkContinuousIndex
  itkCovariantVector
  itkVector
  itkRGBPixel
  itkVariableLengthVector
  itkImage
  itkVectorImage
  itkLevelSet
  itkDefaultStaticMeshTraits
  itkNeighborhood
  ITKRegions
#  SwigExtras
  itkImageSource
  itkImageToImageFilter
  itkInPlaceImageFilter
  itkFiniteDifferenceImageFilter
)

SET(WRAPPER_LIBRARY_DEPENDS VXLNumerics)

SET(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()

WRAPPER_LIBRARY_CREATE_LIBRARY()



