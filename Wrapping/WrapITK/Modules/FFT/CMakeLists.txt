BEGIN_WRAPPER_LIBRARY(FFT)

set(WRAPPER_LIBRARY_DEPENDS Base)

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

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

