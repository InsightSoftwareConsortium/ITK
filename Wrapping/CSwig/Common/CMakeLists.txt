# create a list of cable config files for wrapping
SET(WRAP_SOURCES
  wrap_ITKCommonBase
  wrap_ITKInterpolators
  wrap_ITKKernelDeformableTransforms
  wrap_ITKRegions
  wrap_ITKRigidTransforms
  wrap_ITKSimilarityTransforms
  wrap_SwigExtras
  wrap_itkArray
  wrap_itkAffineTransform
  wrap_itkAzimuthElevationToCartesianTransform
  wrap_itkBSplineDeformableTransform
  wrap_itkContinuousIndex
  wrap_itkDenseFiniteDifferenceImageFilter
  wrap_itkDifferenceImageFilter
  wrap_itkEventObject
  wrap_itkFiniteDifferenceImageFilter
  wrap_itkFixedArray
  wrap_itkIdentityTransform
  wrap_itkImage_2D
  wrap_itkImage_3D
  wrap_itkImageSource
  wrap_itkImageToImageFilter
  wrap_itkIndex
  wrap_itkLevelSet
  wrap_itkPoint
  wrap_itkSize
  wrap_itkScaleTransform
  wrap_itkTransform
  wrap_itkTranslationTransform
  wrap_itkVector
  wrap_itkVersorTransform
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommon.mdx"
)
SET(ITK_SWIG_FILE ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i)
SET(ITK_SWIG_FILE_CXX ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrap)
SET(ITK_EXTRA_TCL_SOURCES itkTclCommand.cxx)
SET(ITK_EXTRA_PYTHON_SOURCES itkPyCommand.cxx)
SET(ITK_EXTRA_PYTHON_WRAP wrap_ITKPyUtils)
SET(ITK_EXTRA_TCL_WRAP wrap_ITKUtils)
ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKCommon Common "_VXLNumericsPython" itkStringStream.cxx "")

