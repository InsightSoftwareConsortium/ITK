BEGIN_WRAPPER_LIBRARY(BaseTransforms)

set(WRAPPER_LIBRARY_GROUPS
  itkTransform
  itkMatrixOffsetTransformBase
  ITKRigidTransforms
  ITKEulerTransform
  itkVersorTransform
  ITKVersorRigidTransform
  ITKSimilarityTransforms
  itkAffineTransform
  itkScalableAffineTransform
  itkScaleTransform
)

set(WRAPPER_LIBRARY_DEPENDS Base)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()
