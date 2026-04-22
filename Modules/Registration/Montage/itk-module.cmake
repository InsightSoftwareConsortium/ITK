set(
  DOCUMENTATION
  "This module provides Montage: mosaic-stitching and 3D
reconstruction of large datasets from a collection of partially
overlapping 2D slices via phase-correlation image registration.
Core pieces are \\\\ref PhaseCorrelationImageRegistrationMethod,
\\\\ref TileMontage, and \\\\ref TileMergeImageFilter.
See the module README for in-tree vs archived-upstream scope."
)

itk_module(
  Montage
  DEPENDS
    ITKCommon
    ITKFFT
    ITKTransform
    ITKIOImageBase
    ITKImageFrequency
    ITKDoubleConversion
  TEST_DEPENDS
    ITKIOTransformInsightLegacy
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
