IF (USE_FFTW)
SET(FFTW_LIB_SEARCHPATH
  /usr/lib
  /usr/lib/fftw
  /usr/local/lib
  /usr/local/lib/fftw
)

FIND_LIBRARY(FFTWD_LIB fftw3 ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib
FIND_LIBRARY(FFTWF_LIB fftw3f ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib

IF(FFTWD_LIB)
  SET(FFTWD_FOUND 1)
  LINK_LIBRARIES( ${FFTWD_LIB} )
  ADD_DEFINITIONS(-DUSE_FFTWD)
ENDIF(FFTWD_LIB)

IF(FFTWF_LIB)
  SET(FFTWF_FOUND 1)
  LINK_LIBRARIES( ${FFTWF_LIB} )
  ADD_DEFINITIONS(-DUSE_FFTWF)
ENDIF(FFTWF_LIB)

IF(FFTWD_LIB AND FFTWF_LIB)
  ADD_DEFINITIONS(-DUSE_FFTW) #Requires that both double and single precision are available
  SET(FFTW_LIB ${FFTWD_LIB} ${FFTWF_LIB}) #For backwards compatibility
ENDIF(FFTWD_LIB AND FFTWF_LIB)

SET(FFTW_INC_SEARCHPATH
  /sw/include
  /usr/include
  /usr/include/fftw
  /usr/local/include
  /usr/local/include/fftw
)

FIND_PATH(FFTW_INCLUDE_PATH fftw3.h ${FFTW_INC_SEARCHPATH})

IF(FFTW_INCLUDE_PATH)
  SET(FFTW_INCLUDE ${FFTW_INCLUDE_PATH})
ENDIF (FFTW_INCLUDE_PATH)

IF(FFTW_INCLUDE)
  INCLUDE_DIRECTORIES( ${FFTW_INCLUDE})
ENDIF(FFTW_INCLUDE)

MARK_AS_ADVANCED(FFTW_INCLUDE_PATH FFTWD_LIB FFTWF_LIB)
OPTION(FFTW_INCLUDE_PATH "The base path of the directory that includes fftw3.h" )
OPTION(FFTWD_LIB "The full path to the fftw3 library (including the library)" )
OPTION(FFTWF_LIB "The full path to the fftw3f library (including the library)" )

ENDIF (USE_FFTW)
