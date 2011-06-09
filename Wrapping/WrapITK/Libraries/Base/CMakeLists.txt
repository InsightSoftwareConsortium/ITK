WRAP_LIBRARY(Base)

set(WRAPPER_LIBRARY_GROUPS
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
  itkImageRegion
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

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()

END_WRAP_LIBRARY()
