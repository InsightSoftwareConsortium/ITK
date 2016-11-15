set(ITKFFT_SRCS
  itkFFTWGlobalConfiguration.cxx
  )

# this library is only needed if FFTW is used
if( ITK_USE_FFTWF OR ITK_USE_FFTWD )
  itk_module_add_library(ITKFFT ${ITKFFT_SRCS})

  if(ITK_USE_FFTWD)
    if(ITK_USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWD_LIB})
      if(FFTWD_THREADS_LIB)
        target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWD_THREADS_LIB})
      endif()
    else()
      add_dependencies(ITKFFT fftwd)
      target_link_libraries(ITKFFT LINK_PUBLIC fftw3 fftw3_threads)
    endif()
  endif()

  if(ITK_USE_FFTWF)
    if(ITK_USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWF_LIB})
      if(FFTWF_THREADS_LIB)
        target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWF_THREADS_LIB})
      endif()
    else()
      add_dependencies(ITKFFT fftwf)
      target_link_libraries(ITKFFT LINK_PUBLIC fftw3f fftw3f_threads)
    endif()
  endif()
endif()
