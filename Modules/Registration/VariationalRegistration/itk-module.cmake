# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in VariationalRegistration
# The testing module in VariationalRegistration depends on ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  VariationalRegistration
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKImageFilterBase
    ITKSmoothing
    ITKFFT
    ITKFiniteDifference
    ITKDisplacementField
    ITKRegistrationCommon
    ITKMathematicalMorphology
    ITKBinaryMathematicalMorphology
  PRIVATE_DEPENDS
    ITKIOGDCM
    ITKIOMeta
    ITKIOJPEG
    ITKIOPNG
    ITKIOTIFF
    ITKIOBMP
    ITKIOVTK
    ITKIONRRD
    ITKIOGIPL
    ITKIONIFTI
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "Diffeomorphic, demons, and curvature-based variational image registration with multi-resolution support."
  EXCLUDE_FROM_DEFAULT
  # Only use with libraries with compiled source ENABLE_SHARED
)
