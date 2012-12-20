project(ITKFFT)

# this library is only needed if FFTW is used
if( ITK_USE_FFTWF OR ITK_USE_FFTWD )
  set(ITKFFT_LIBRARIES ITKFFT)
endif()
itk_module_impl()
