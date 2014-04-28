set(
  DOCUMENTATION
  "This module contains a collection of classes for performing
variational image registration."
)

itk_module(
  VariationalRegistration
  DEPENDS
    ITKCommon
    ITKIOPNG
    ITKIONIFTI
    ITKIOImageBase
    ITKImageFilterBase
    ITKSmoothing
    ITKFFT
    ITKFiniteDifference
    ITKDisplacementField
    ITKRegistrationCommon
    ITKMathematicalMorphology
    ITKBinaryMathematicalMorphology
  TEST_DEPENDS
    ITKIOImageBase
    ITKTestKernel #to handle IO in src
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
