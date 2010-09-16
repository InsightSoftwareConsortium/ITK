BEGIN_WRAPPER_LIBRARY(Registration)

set(WRAPPER_LIBRARY_GROUPS
  itkPDEDeformableRegistrationFilter
  itkPDEDeformableRegistrationFunction
  itkESMDemonsRegistrationFunction
  itkDisplacementFieldJacobianDeterminantFilter
)

set(WRAPPER_LIBRARY_DEPENDS
  Base
  BaseNumerics
  BaseTransforms
  BaseSpatialObject)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

