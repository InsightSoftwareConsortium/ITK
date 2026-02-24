project(ITKFFT)

if(ITK_USE_CUFFTW)
  set(
    ITKFFT_LIBRARIES
    ITKFFT
    ${FFTWF_LIBRARIES}
  )
else()
  set(ITKFFT_LIBRARIES ITKFFT)
endif()

itk_module_impl()
