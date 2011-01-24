WRAP_LIBRARY(Base)

set(WRAPPER_LIBRARY_GROUPS
  itkEventObjects
  ITKCommonBase
  itkOffset
  itkArray
  itkFixedArray
  itkIndex
  itkSize
  itkVector
  itkPoint
#  itkDefaultDynamicMeshTraits
#  itkDefaultStaticMeshTraits
  itkLevelSet
  itkContinuousIndex
  itkVectorContainer
  itkMapContainer
  itkBoundingBox
#  itkPointSet
  itkCovariantVector
  itkRGBPixel
  itkRGBAPixel
  itkVariableLengthVector
  ITKRegions
  itkMatrix
  itkSymmetricSecondRankTensor
  itkImage
  itkVectorImage
  itkNeighborhood
#  SwigExtras
  itkImageSource
  itkImageToImageFilterA
  itkImageToImageFilterB
  itkInPlaceImageFilterA
  itkInPlaceImageFilterB
  itkFiniteDifferenceFunction
  itkFiniteDifferenceImageFilter
  itkPath
  itkParametricPath
  itkPolyLineParametricPath
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics)

AUTO_INCLUDE_MODULES()

END_WRAP_LIBRARY()
