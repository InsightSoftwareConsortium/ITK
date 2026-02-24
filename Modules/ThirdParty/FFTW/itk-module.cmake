set(
  DOCUMENTATION
  "This module contains the third party <a href=\"http://www.fftw.org\">FFTW</a> library.
FFTW is a C library for computing the discrete Fourier transform (DFT) in one or more dimensions.
Note: FFTW is licensed under the GPL. By enabling this module, your ITK build will be covered by the GPL license."
)

itk_module(ITKFFTW DEPENDS DESCRIPTION "${DOCUMENTATION}" EXCLUDE_FROM_DEFAULT)
