WRAP_LIBRARY(Registration)

set(WRAPPER_LIBRARY_GROUPS
  itkPDEDeformableRegistrationFilter
  itkPDEDeformableRegistrationFunction
  itkESMDemonsRegistrationFunction
  itkDisplacementFieldJacobianDeterminantFilter
)

set(WRAPPER_LIBRARY_DEPENDS
  VXLNumerics
  Base
  Numerics
  Transforms
  SpatialObject
  Resize
  Calculators
)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics)

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
