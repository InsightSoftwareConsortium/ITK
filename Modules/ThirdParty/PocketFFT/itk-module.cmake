set(
  DOCUMENTATION
  "This module contains the third party <a href=\"https://github.com/mreineck/pocketfft\">pocketfft</a> library.
pocketfft is a header-only C++ Fast Fourier Transform implementation used as a default FFT backend in ITK."
)

itk_module(ITKPocketFFT DEPENDS DESCRIPTION "${DOCUMENTATION}")
