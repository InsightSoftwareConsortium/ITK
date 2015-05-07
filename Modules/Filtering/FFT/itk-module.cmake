set(DOCUMENTATION "This module provides interfaces to FFT
implementations. In particular it provides the direct and inverse
computations of Fast Fourier Transforms based on
<a href=\"http://vxl.sourceforge.net/\">VXL</a> and
<a href=\"http://www.fftw.org\">FFTW</a>. Note that when using the FFTW
implementation you must comply with the GPL license.")

if( ITK_USE_FFTWF OR ITK_USE_FFTWD )
  set(FFT_ENABLE_SHARED "ENABLE_SHARED")
endif()

itk_module(ITKFFT
  ${FFT_ENABLE_SHARED}
  DEPENDS
    ITKCommon
    ITKImageGrid
  TEST_DEPENDS
    ITKImageCompare
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
