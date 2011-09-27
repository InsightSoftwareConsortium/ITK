set(ITKFFT_SRC
itkFFTWGlobalConfiguration.cxx
)

# this library is only needed if FFTW is used
if( USE_FFTWF OR USE_FFTWD )
  add_library(ITKFFT ${ITKFFT_SRC})
  target_link_libraries(ITKFFT  ${ITKCommon_LIBRARIES})

  if(USE_FFTWD)
    add_dependencies(ITKFFT fftwd)
    target_link_libraries(ITKFFT fftw3 fftw3_threads)
  endif(USE_FFTWD)
  if(USE_FFTWF)
    add_dependencies(ITKFFT fftwf)
    target_link_libraries(ITKFFT fftw3f fftw3f_threads )
  endif(USE_FFTWF)

  itk_module_target(ITKFFT)
endif()
