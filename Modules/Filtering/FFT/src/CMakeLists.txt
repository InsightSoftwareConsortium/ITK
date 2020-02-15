set(ITKFFT_SRCS
  itkFFTWGlobalConfiguration.cxx
  itkComplexToComplexFFTImageFilter.cxx
  )

itk_module_add_library(ITKFFT ${ITKFFT_SRCS})

# this library is only needed if FFTW is used
if( ITK_USE_FFTWF OR ITK_USE_FFTWD AND NOT ITK_USE_CUFFTW)
  if(ITK_USE_FFTWD)
    if(ITK_USE_SYSTEM_FFTW)
      target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWD_LIB})
      if(FFTWD_THREADS_LIB)
        target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWD_THREADS_LIB})
      endif()
    else()
      add_dependencies(ITKFFT fftwd)
      target_link_libraries(ITKFFT LINK_PUBLIC fftw3_threads fftw3)
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
      target_link_libraries(ITKFFT LINK_PUBLIC fftw3f_threads fftw3f)
    endif()
  endif()
endif()
