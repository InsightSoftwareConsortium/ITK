WRAP_LIBRARY(ITKCommon)

set(WRAPPER_LIBRARY_GROUPS
  vcl_complex
  vnl_matrix
  vnl_vector
  vnl_unary_function
  ITKCommonBase
  itkOffset
  itkArray
  itkFixedArray
  itkIndex
  itkSize
  itkVector
  itkPoint
  itkContinuousIndex
  itkVectorContainer
  itkMapContainer
  itkBoundingBox
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
  itkDefaultDynamicMeshTraits
  itkDefaultStaticMeshTraits
#  itkCellInterface
#  itkPointSet
#  itkVertexCell
#  itkLineCell
)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()

END_WRAP_LIBRARY()
