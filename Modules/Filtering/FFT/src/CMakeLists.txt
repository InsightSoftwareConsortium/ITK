set(ITKFFT_SRCS
itkComplexToComplexFFTImageFilter.cxx
itkVnlFFTImageFilterInitFactory.cxx)

if(ITK_USE_FFTWF OR ITK_USE_FFTWD AND NOT ITK_USE_CUFFTW)
  list(APPEND ITKFFT_SRCS itkFFTWGlobalConfiguration.cxx)
  list(APPEND ITKFFT_SRCS itkFFTWFFTImageFilterInitFactory.cxx)
endif()

itk_module_add_library(ITKFFT ${ITKFFT_SRCS})

# this library is only needed if FFTW is used
if(ITK_USE_FFTWF OR ITK_USE_FFTWD AND NOT ITK_USE_CUFFTW)

  if(ITK_USE_FFTWD)
    if(NOT ITK_USE_SYSTEM_FFTW)
      add_dependencies(ITKFFT fftwd) # Ensure that local FFTW is built before using
      target_link_libraries(ITKFFT LINK_PUBLIC ${ITK_FFTWD_LIBRARIES_NAMES})
    else()
      target_include_directories(ITKFFT PUBLIC ${FFTW_INCLUDE})
      target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWD_LIBRARIES})
    endif()
  endif()

  if(ITK_USE_FFTWF)
    if(NOT ITK_USE_SYSTEM_FFTW)
      add_dependencies(ITKFFT fftwf) # Esnsure that local FFTW is build before using
      target_link_libraries(ITKFFT LINK_PUBLIC ${ITK_FFTWF_LIBRARIES_NAMES})
    else()
      target_include_directories(ITKFFT PUBLIC ${FFTW_INCLUDE})
      target_link_libraries(ITKFFT LINK_PUBLIC ${FFTWF_LIBRARIES})
    endif()
  endif()
endif()
