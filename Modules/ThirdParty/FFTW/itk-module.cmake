set(
  DOCUMENTATION
  "This module provides FFTW, a GPL-licensed fast Fourier transform library,
with optional single and double precision support."
)

itk_module(
  ITKFFTW
  EXCLUDE_FROM_DEFAULT
  TEST_DEPENDS
    ITKGoogleTest
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
