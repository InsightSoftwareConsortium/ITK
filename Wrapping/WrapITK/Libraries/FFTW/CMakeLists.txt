
include(FindFFTW.cmake)

WRAP_LIBRARY("FFTW")
set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base FFT)

# find needed libs
set(libs ITKCommon ITKAlgorithms)

if(WRAP_complex_float AND WRAP_float)
  if(FFTWF_LIB)
    set(libs ${libs} ${FFTWF_LIB})
  else(FFTWF_LIB)
    message("Warning: FFTWF not found automatically, some or all FFTW filters will not be wrapped. Set FFTWF parameters manually and try again.")
  endif(FFTWF_LIB)
endif(WRAP_complex_float AND WRAP_float)

if(WRAP_complex_double AND WRAP_double)
  if(FFTWD_LIB)
    set(libs ${libs} ${FFTWD_LIB})
  else(FFTWD_LIB)
    message("Warning: FFTWD not found automatically, some or all FFTW filters will not be wrapped. Set FFTWD parameters manually and try again.")
  endif(FFTWD_LIB)
endif(WRAP_complex_double AND WRAP_double)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
