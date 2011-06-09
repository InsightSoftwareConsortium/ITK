WRAP_LIBRARY(Transforms)

set(WRAPPER_LIBRARY_GROUPS
  itkTransform
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
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
