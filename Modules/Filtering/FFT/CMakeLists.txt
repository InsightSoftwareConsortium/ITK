project(ITK-FFT)

# this library is only needed if FFTW is used
if( USE_FFTWF OR USE_FFTWD )
  set(ITK-FFT_LIBRARIES ITK-FFT)
endif()
itk_module_impl()