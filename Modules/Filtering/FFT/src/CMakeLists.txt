set(ITKFFT_SRC
itkFFTWGlobalConfiguration.cxx
)

# this library is only needed if FFTW is used
if( ITK_USE_FFTWF OR ITK_USE_FFTWD )
  add_library(ITKFFT ${ITKFFT_SRC})
  target_link_libraries(ITKFFT  ${ITKCommon_LIBRARIES})

  if(ITK_USE_FFTWD)
    if(ITK_USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT ${FFTWD_LIB})
      if(FFTWD_THREADS_LIB)
        target_link_libraries(ITKFFT ${FFTWD_THREADS_LIB})
      endif()
    else()
      add_dependencies(ITKFFT fftwd)
      target_link_libraries(ITKFFT fftw3 fftw3_threads)
    endif()
  endif()

  if(ITK_USE_FFTWF)
    if(ITK_USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT ${FFTWF_LIB})
      if(FFTWF_THREADS_LIB)
        target_link_libraries(ITKFFT ${FFTWF_THREADS_LIB})
      endif()
    else()
      add_dependencies(ITKFFT fftwf)
      target_link_libraries(ITKFFT fftw3f fftw3f_threads)
    endif()
  endif()

  itk_module_target(ITKFFT)
endif()
