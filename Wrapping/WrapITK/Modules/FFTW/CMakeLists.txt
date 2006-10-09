# FIND_PACKAGE(FFTW)

BEGIN_WRAPPER_LIBRARY("FFTW")
SET(WRAPPER_LIBRARY_DEPENDS Base FFT)

# find needed libs
SET(libs ITKCommon ITKAlgorithms)

IF(WRAP_complex_float AND WRAP_float)
  IF(FFTWF_LIB)
    SET(libs ${libs} ${FFTWF_LIB})
  ELSE(FFTWF_LIB)
    MESSAGE("Warning: FFTWF not found. Some FFTW filters will not be wrapped")
  ENDIF(FFTWF_LIB)
ENDIF(WRAP_complex_float AND WRAP_float)

IF(WRAP_complex_double AND WRAP_double)
  IF(FFTWD_LIB)
    SET(libs ${libs} ${FFTWD_LIB})
  ELSE(FFTWD_LIB)
    MESSAGE("Warning: FFTWD not found. Some FFTW filters will not be wrapped")
  ENDIF(FFTWD_LIB)
ENDIF(WRAP_complex_double AND WRAP_double)

SET(WRAPPER_LIBRARY_LINK_LIBRARIES ${libs})

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()


