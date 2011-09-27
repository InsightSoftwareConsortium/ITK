project(ITKFFT)

# this library is only needed if FFTW is used
if( USE_FFTWF OR USE_FFTWD )
  set(ITKFFT_LIBRARIES ITKFFT)
endif()
itk_module_impl()