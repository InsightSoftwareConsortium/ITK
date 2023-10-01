set(DOCUMENTATION
    "This module provides interfaces to FFT
implementations. In particular it provides the direct and inverse
computations of Fast Fourier Transforms based on
<a href=\"http://vxl.sourceforge.net/\">VXL</a> and
<a href=\"https://www.fftw.org\">FFTW</a>. Note that when using the FFTW
implementation you must comply with the GPL license.")

set(_fft_backends "FFTImageFilterInit::Vnl")
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  # Prepend so that FFTW constructor is preferred
  list(PREPEND _fft_backends "FFTImageFilterInit::FFTW")
endif()

itk_module(
  ITKFFT
  ENABLE_SHARED
  DEPENDS
  ITKCommon
  COMPILE_DEPENDS
  ITKImageGrid
  TEST_DEPENDS
  ITKTestKernel
  ITKImageCompare
  ITKImageIntensity
  FACTORY_NAMES
  ${_fft_backends}
  DESCRIPTION
  "${DOCUMENTATION}")
