itk_wrap_module(ITKCommon)

set(WRAPPER_SUBMODULE_ORDER
  itkFixedArray
  itkVector
  itkCovariantVector
  itkPoint
  itkTriangleHelper
  itkVariableSizeMatrix
  itkVersor
  stdcomplex
  vnl_c_vector
  vnl_cost_function
  vnl_diag_matrix
  vnl_file_matrix
  vnl_file_vector
  vnl_fortran_copy
  vnl_least_squares_function
  vnl_matrix_fixed
  vnl_matrix_fixed_ref
  vnl_matrix_ref
  vnl_vector_ref
  vnl_matrix
  vnl_vector
  vnl_unary_function
  ITKCommonBase
  itkSize
  itkOffset
  itkArray
  itkIndex
  itkContinuousIndex
  itkMatrix
  itkVectorContainer
  itkMapContainer
  itkBoundingBox
  itkRGBPixel
  itkRGBAPixel
  itkVariableLengthVector
  itkImageRegion
  itkSymmetricSecondRankTensor
  itkImage
  itkVectorImage
  itkNeighborhood
#  SwigExtras
  itkImageSourceCommon
  itkImageSource
  itkImageToImageFilterCommon
  itkImageToImageFilterA
  itkImageToImageFilterB
  itkInPlaceImageFilterA
  itkInPlaceImageFilterB
  itkDefaultDynamicMeshTraits
  itkDefaultStaticMeshTraits
  itkFunctionBase
#  itkCellInterface
#  itkPointSet
#  itkVertexCell
#  itkLineCell
)
itk_auto_load_submodules()

itk_end_wrap_module()
