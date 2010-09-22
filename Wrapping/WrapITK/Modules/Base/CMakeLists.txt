BEGIN_WRAPPER_LIBRARY(Base)

set(WRAPPER_LIBRARY_GROUPS
  ITKCommonBase
  itkOffset
  itkArray
  itkFixedArray
  itkPoint
  itkDefaultStaticMeshTraits
  itkDefaultDynamicMeshTraits
  itkPointSet
  itkContinuousIndex
  itkCovariantVector
  itkVector
  itkRGBPixel
  itkRGBAPixel
  itkVariableLengthVector
  itkImage
  itkVectorImage
  itkLevelSet
  itkNeighborhood
  ITKRegions
  SwigExtras
  itkImageSource
  itkImageToImageFilterA
  itkImageToImageFilterB
  itkInPlaceImageFilterA
  itkInPlaceImageFilterB
  itkFiniteDifferenceImageFilter
  itkPath
  itkParametricPath
  itkPolyLineParametricPath
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics)

set(WRAPPER_LIBRARY_SWIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/SwigExtras.i")
set(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/itkStringStream.cxx")

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()

WRAPPER_LIBRARY_CREATE_LIBRARY()



