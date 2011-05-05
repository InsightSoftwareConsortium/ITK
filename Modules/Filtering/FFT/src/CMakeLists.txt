set(ITK-FFT_SRC
itkFFTWGlobalConfiguration.cxx
)

add_library(ITK-FFT ${ITK-FFT_SRC})
target_link_libraries(ITK-FFT  ${ITK-Common_LIBRARIES})

if(USE_FFTWD)
  add_dependencies(ITK-FFT fftwd)
  target_link_libraries(ITK-FFT fftw3 fftw3_threads)
endif(USE_FFTWD)
if(USE_FFTWF)
  add_dependencies(ITK-FFT fftwf)
  target_link_libraries(ITK-FFT fftw3f fftw3f_threads )
endif(USE_FFTWF)

itk_module_target(ITK-FFT)
