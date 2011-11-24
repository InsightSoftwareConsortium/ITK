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
AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
