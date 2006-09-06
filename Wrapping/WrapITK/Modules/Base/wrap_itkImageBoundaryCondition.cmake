# gcc-xml on windows cannot be allowed to see the full ConstantBoundaryCondition
# header because currently gcc-xml and cswig cannot properly wrap functions
# that use __declspec specifiers, which one part of itkConstantBoundaryCondition.h
# does on windows. Just pass the stub header which will keep gcc-xml away from the 
# real header on windows.
# IF(WIN32)
#   WRAP_INCLUDE("itkConstantBoundaryConditionStub.h")
# ENDIF(WIN32)

# WRAP_CLASS("itk::ImageBoundaryCondition")
#   WRAP_IMAGE_FILTER_USIGN_INT(1)
#   WRAP_IMAGE_FILTER_SIGN_INT(1)
#   WRAP_IMAGE_FILTER_REAL(1)
#   WRAP_IMAGE_FILTER_VECTOR_REAL(1)
#   WRAP_IMAGE_FILTER_COV_VECTOR_REAL(1)
# END_WRAP_CLASS()

WRAP_CLASS("itk::PeriodicBoundaryCondition")
  WRAP_IMAGE_FILTER_USIGN_INT(1)
  WRAP_IMAGE_FILTER_SIGN_INT(1)
  WRAP_IMAGE_FILTER_REAL(1)
  WRAP_IMAGE_FILTER_VECTOR_REAL(1)
  WRAP_IMAGE_FILTER_COV_VECTOR_REAL(1)
  WRAP_IMAGE_FILTER_RGB(1)
END_WRAP_CLASS()

WRAP_CLASS("itk::ZeroFluxNeumannBoundaryCondition")
  WRAP_IMAGE_FILTER_USIGN_INT(1)
  WRAP_IMAGE_FILTER_SIGN_INT(1)
  WRAP_IMAGE_FILTER_REAL(1)
  WRAP_IMAGE_FILTER_VECTOR_REAL(1)
  WRAP_IMAGE_FILTER_COV_VECTOR_REAL(1)
  WRAP_IMAGE_FILTER_RGB(1)
END_WRAP_CLASS()

WRAP_CLASS("itk::ConstantBoundaryCondition")
  WRAP_IMAGE_FILTER_USIGN_INT(1)
  WRAP_IMAGE_FILTER_SIGN_INT(1)
  WRAP_IMAGE_FILTER_REAL(1)
  WRAP_IMAGE_FILTER_VECTOR_REAL(1)
  WRAP_IMAGE_FILTER_COV_VECTOR_REAL(1)
  WRAP_IMAGE_FILTER_RGB(1)
END_WRAP_CLASS()
