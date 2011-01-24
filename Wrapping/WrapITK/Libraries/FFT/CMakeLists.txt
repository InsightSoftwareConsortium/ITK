WRAP_LIBRARY(FFT)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base)

# find needed libs
set(libs ITKCommon ITKAlgorithms)

if(WRAP_complex_float AND WRAP_float)
  if(FFTWF_LIB)
    set(libs ${libs} ${FFTWF_LIB})
  endif(FFTWF_LIB)
endif(WRAP_complex_float AND WRAP_float)

if(WRAP_complex_double AND WRAP_double)
  if(FFTWD_LIB)
    set(libs ${libs} ${FFTWD_LIB})
  endif(FFTWD_LIB)
endif(WRAP_complex_double AND WRAP_double)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${libs})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
