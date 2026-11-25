set(
  DOCUMENTATION
  "This module contains an implementation of the
principal component analysis for the ITK toolkit."
)

itk_module(
  PrincipalComponentsAnalysis
  DEPENDS
    ITKCommon
    ITKMesh
    ITKIOMesh
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
