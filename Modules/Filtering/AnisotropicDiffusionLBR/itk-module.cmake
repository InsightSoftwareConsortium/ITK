set(DOCUMENTATION "This module implements Anisotropic Diffusion, using Lattice Basis Reduction.")

# itk_module() defines the module dependencies in ITKAnisotropicFastMarchingLBR;
# ITKAnisotropicFastMarchingLBR depends on ITKCommon;
# The testing module in ITKExternalTemplate depends on ITKTestKernel,
# and ITKMetaIO for image IO (besides ITKAnisotropicDiffusionLBR itself)

itk_module(ITKAnisotropicDiffusionLBR
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKIOSpatialObjects
    ITKMetaIO
    ITKImageGradient
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}"
)
