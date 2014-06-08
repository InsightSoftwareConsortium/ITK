set(
  DOCUMENTATION
  "This module contains classes for performing phase symmetry
filtration. For more information, see
\"Multi-scale Steerable Phase-Symmetry Filters for ITK\" by C. Hatt.
http://www.insight-journal.org/browse/publication/846
http://hdl.handle.net/10380/3330
"
)

itk_module(
  PhaseSymmetry
  DEPENDS
    ITKCommon
    ITKFFT
    ITKImageIntensity
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
