set(
  ITKFFT_SRCS
  itkComplexToComplexFFTImageFilter.cxx
  itkVnlFFTImageFilterInitFactory.cxx
)

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  list(APPEND ITKFFT_SRCS itkFFTWFFTImageFilterInitFactory.cxx)
  if(NOT ITK_USE_CUFFTW)
    list(APPEND ITKFFT_SRCS itkFFTWGlobalConfiguration.cxx)
  endif()
endif()

itk_module_add_library(ITKFFT ${ITKFFT_SRCS})
