# create a list of cable config files for wrapping
SET(WRAP_SOURCES
  wrap_ITKKernelDeformableTransforms
  wrap_ITKRigidTransforms
  wrap_ITKSimilarityTransforms
  wrap_itkAffineTransform
  wrap_itkAzimuthElevationToCartesianTransform
  wrap_itkBSplineDeformableTransform
  wrap_itkIdentityTransform
  wrap_itkMatrixOffsetTransformBase
  wrap_itkScaleTransform
  wrap_itkTransform
  wrap_itkTranslationTransform
  wrap_itkVersorTransform
)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx"
                       "${WrapITK_BINARY_DIR}/CommonA/ITKCommonA.mdx"
                       "${WrapITK_BINARY_DIR}/CommonB/ITKCommonB.mdx"
)

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" ITKCommonB CommonB "ITKCommonA;VXLNumerics" "" "ITKCommon")

