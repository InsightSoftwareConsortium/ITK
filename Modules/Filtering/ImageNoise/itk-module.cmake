set(DOCUMENTATION "This module con stains classes to simulate and
evaluate noise. The classes were originally contributed via the
Insight Journal (http://hdl.handle.net/10380/3158).")

itk_module(ITKImageNoise
  DEPENDS
    ITKImageFilterBase
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
