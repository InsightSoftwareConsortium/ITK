set(DOCUMENTATION "This module contains classes to simulate and
evaluate noise. These classes were originally contributed via the
Insight Journal (https://www.insight-journal.org/browse/publication/721).")

itk_module(ITKImageNoise
  COMPILE_DEPENDS
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
