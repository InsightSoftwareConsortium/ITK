# create a list of cable config files for wrapping
SET(WRAP_SOURCES
  wrap_ITKCommonBase
  wrap_ITKInterpolators
  wrap_ITKRegions
  wrap_SwigExtras
  wrap_itkArray
  wrap_itkBinaryBallStructuringElement
  wrap_itkContinuousIndex
  wrap_itkDenseFiniteDifferenceImageFilter_2D
  wrap_itkDenseFiniteDifferenceImageFilter_3D
  wrap_itkDifferenceImageFilter
  wrap_itkEventObject
  wrap_itkFiniteDifferenceFunction
  wrap_itkFiniteDifferenceImageFilter_2D
  wrap_itkFiniteDifferenceImageFilter_3D
  wrap_itkFixedArray
  wrap_itkFunctionBase
  wrap_itkImage_2D
  wrap_itkImage_3D
  wrap_itkImageFunction
  wrap_itkImageConstIterator
  wrap_itkImageRegionIterator
  wrap_itkImageRegionConstIterator
  wrap_itkImageSource
  wrap_itkImageToImageFilter_2D
  wrap_itkImageToImageFilter_3D
  wrap_itkInPlaceImageFilter_A
  wrap_itkInPlaceImageFilter_B
  wrap_itkIndex
  wrap_itkLevelSet
  wrap_itkNeighborhood
  wrap_itkPoint
  wrap_itkSize
  wrap_itkVector
)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
)
SET(ITK_SWIG_FILE ${ITK_TOP}/Wrapping/CSwig/CommonA/SwigExtras.i)
SET(ITK_SWIG_FILE_CXX ${WrapITK_BINARY_DIR}/CommonA/SwigExtras_wrap)
SET(ITK_EXTRA_TCL_SOURCES itkTclCommand.cxx)
SET(ITK_EXTRA_PYTHON_SOURCES itkPyCommand.cxx)
SET(ITK_EXTRA_PYTHON_WRAP wrap_ITKPyUtils )

IF(PYTHON_NUMARRAY_FOUND)
  SET(ITK_EXTRA_PYTHON_WRAP ${ITK_EXTRA_PYTHON_WRAP} wrap_itkPyBuffer )
ENDIF(PYTHON_NUMARRAY_FOUND)

SET(ITK_EXTRA_TCL_WRAP wrap_ITKUtils)
ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKCommonA CommonA "VXLNumerics" itkStringStream.cxx "ITKCommon")

