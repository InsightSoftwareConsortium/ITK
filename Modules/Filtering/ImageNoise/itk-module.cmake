set(DOCUMENTATION
    "This module contains classes to simulate and
evaluate noise. These classes were originally contributed via the
Insight Journal (https://doi.org/10.54294/vh6vbw).")

itk_module(
  ITKImageNoise
  COMPILE_DEPENDS
  ITKStatistics
  TEST_DEPENDS
  ITKTestKernel
  DESCRIPTION
  "${DOCUMENTATION}")
