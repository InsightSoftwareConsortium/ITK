# find_package(FFTW)

BEGIN_WRAPPER_LIBRARY("FFTW")
set(WRAPPER_LIBRARY_DEPENDS Base FFT)

# find needed libs
set(libs ITKCommon ITKAlgorithms)

if(WRAP_complex_float AND WRAP_float)
  if(FFTWF_LIB)
    set(libs ${libs} ${FFTWF_LIB})
  else(FFTWF_LIB)
    message("Warning: FFTWF not found. Some FFTW filters will not be wrapped")
  endif(FFTWF_LIB)
endif(WRAP_complex_float AND WRAP_float)

if(WRAP_complex_double AND WRAP_double)
  if(FFTWD_LIB)
    set(libs ${libs} ${FFTWD_LIB})
  else(FFTWD_LIB)
    message("Warning: FFTWD not found. Some FFTW filters will not be wrapped")
  endif(FFTWD_LIB)
endif(WRAP_complex_double AND WRAP_double)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${libs})

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()


