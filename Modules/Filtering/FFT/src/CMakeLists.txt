set(ITKFFT_SRC
itkFFTWGlobalConfiguration.cxx
)

# this library is only needed if FFTW is used
if( USE_FFTWF OR USE_FFTWD )
  add_library(ITKFFT ${ITKFFT_SRC})
  target_link_libraries(ITKFFT  ${ITKCommon_LIBRARIES})

  if(USE_FFTWD)
    if(USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT ${FFTWD_LIB})
      if(FFTWD_THREADS_LIB)
        target_link_libraries(ITKFFT ${FFTWD_THREADS_LIB})
      endif(FFTWD_THREADS_LIB)
    else(USE_SYSTEM_FFTW)
      add_dependencies(ITKFFT fftwd)
      target_link_libraries(ITKFFT fftw3 fftw3_threads)
    endif(USE_SYSTEM_FFTW)
  endif(USE_FFTWD)

  if(USE_FFTWF)
    if(USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT ${FFTWF_LIB})
      if(FFTWF_THREADS_LIB)
        target_link_libraries(ITKFFT ${FFTWF_THREADS_LIB})
      endif(FFTWF_THREADS_LIB)
    else(USE_SYSTEM_FFTW)
      add_dependencies(ITKFFT fftwf)
      target_link_libraries(ITKFFT fftw3f fftw3f_threads)
    endif(USE_SYSTEM_FFTW)
  endif(USE_FFTWF)

  itk_module_target(ITKFFT)
endif()
