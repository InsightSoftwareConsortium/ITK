WRAP_LIBRARY(ITKTransform)

set(WRAPPER_LIBRARY_GROUPS
  itkTransformBase
  itkMatrixOffsetTransformBase
  itkEuler2DTransform
  itkEuler3DTransform
  itkVersorTransform
  itkVersorRigid3DTransform
  itkSimilarity2DTransform
  itkSimilarity3DTransform
  itkAffineTransform
  itkScalableAffineTransform
  itkScaleTransform
  itkKernelTransform
)

set(WRAPPER_LIBRARY_DEPENDS ITKStatistics)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
