BEGIN_WRAPPER_LIBRARY(BaseSpatialObject)

set(WRAPPER_LIBRARY_GROUPS
  itkSpatialObjectBase
  itkPointBasedSpatialObject
  itkImageSpatialObject
  itkTreeNodeSO
)

set(WRAPPER_LIBRARY_DEPENDS Base BaseTransforms)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKSpatialObject)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

