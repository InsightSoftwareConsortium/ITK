# gcc-xml on windows cannot be allowed to see the full ConstantBoundaryCondition
# header because currently gcc-xml and cswig cannot properly wrap functions
# that use __declspec specifiers, which one part of itkConstantBoundaryCondition.h
# does on windows. Just pass the stub header which will keep gcc-xml away from the
# real header on windows.
# if(WIN32)
#   WRAP_INCLUDE("itkConstantBoundaryConditionStub.h")
# endif(WIN32)

WRAP_CLASS("itk::ImageBoundaryCondition")
  WRAP_IMAGE_FILTER_ALL_TYPES(1)
END_WRAP_CLASS()

WRAP_CLASS("itk::PeriodicBoundaryCondition")
  WRAP_IMAGE_FILTER_ALL_TYPES(1)
END_WRAP_CLASS()

WRAP_CLASS("itk::ZeroFluxNeumannBoundaryCondition")
  WRAP_IMAGE_FILTER_ALL_TYPES(1)
END_WRAP_CLASS()

WRAP_CLASS("itk::ConstantBoundaryCondition")
  WRAP_IMAGE_FILTER_ALL_TYPES(1)
END_WRAP_CLASS()
