set(DOCUMENTATION "This module provides interfaces to FFT
implementations. In particular it provides the direct and inverse
computations of Fast Fourier Transforms based on
<a href=\"http://vxl.sourceforge.net/\">VXL</a> and
<a href=\"http://www.fftw.org\">FFTW</a>. Note that when using the FFTW
implementation you must comply with the GPL license.")

set(_fft_backends "FFT::VnlComplexToComplex1D;FFT::VnlComplexToComplex;FFT::VnlForward1D;FFT::VnlForward;FFT::VnlHalfHermitianToRealInverse;FFT::VnlInverse1D;FFT::VnlInverse;FFT::VnlRealToHalfHermitianForward")
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  # Prepend so that FFTW constructor is preferred
  list(PREPEND _fft_backends "FFT::FFTWComplexToComplex1D;FFT::FFTWComplexToComplex;FFT::FFTWForward1D;FFT::FFTWForward;FFT::FFTWHalfHermitianToRealInverse;FFT::FFTWInverse1D;FFT::FFTWInverse;FFT::FFTWRealToHalfHermitianForward")
endif()

itk_module(ITKFFT
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
    "${DOCUMENTATION}"
)
