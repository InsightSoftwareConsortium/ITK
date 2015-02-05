itk_wrap_module(ITKCommon)

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
  itkMatrix
  itkVectorContainer
  itkMapContainer
  itkBoundingBox
  itkCovariantVector
  itkRGBPixel
  itkRGBAPixel
  itkVariableLengthVector
  itkImageRegion
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
itk_auto_load_submodules()

itk_end_wrap_module()
