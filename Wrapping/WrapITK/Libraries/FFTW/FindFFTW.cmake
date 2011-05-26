## FFTW can be compiled and subsequently linked against
## various data types.
## There is a single set of include files, and then muttiple libraries,
## One for each type.  I.e. libfftw.a-->double, libfftwf.a-->float

## The following logic belongs in the individual package
mark_as_advanced(USE_FFTWD)
option(USE_FFTWD "Use double precision FFTW if found" ON)
mark_as_advanced(USE_FFTWF)
option(USE_FFTWF "Use single precision FFTW if found" ON)

if(USE_FFTWD OR USE_FFTWF)

  set(FFTW_INC_SEARCHPATH
    /sw/include
    /usr/include
    /usr/local/include
    /usr/include/fftw
    /usr/local/include/fftw
  )

  find_path(FFTW_INCLUDE_PATH fftw3.h ${FFTW_INC_SEARCHPATH})

  if(FFTW_INCLUDE_PATH)
    set(FFTW_INCLUDE ${FFTW_INCLUDE_PATH})
  endif(FFTW_INCLUDE_PATH)

  if(FFTW_INCLUDE)
    include_directories( ${FFTW_INCLUDE})
  endif(FFTW_INCLUDE)

  get_filename_component(FFTW_INSTALL_BASE_PATH ${FFTW_INCLUDE_PATH} PATH)

  set(FFTW_LIB_SEARCHPATH
    ${FFTW_INSTALL_BASE_PATH}/lib
    /usr/lib/fftw
    /usr/local/lib/fftw
  )

  if(USE_FFTWD)
    mark_as_advanced(FFTWD_LIB)
#   option(FFTWD_LIB "The full path to the fftw3 library (including the library)" )
    find_library(FFTWD_LIB fftw3 ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib
    find_library(FFTWD_THREADS_LIB fftw3_threads ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib only if compiled with threads support

    if(FFTWD_LIB)
      set(FFTWD_FOUND 1)
      if(FFTWD_THREADS_LIB)
        set(FFTWD_LIB ${FFTWD_LIB} ${FFTWD_THREADS_LIB} )
      endif(FFTWD_THREADS_LIB)
    endif(FFTWD_LIB)
  endif(USE_FFTWD)

  if(USE_FFTWF)
    mark_as_advanced(FFTWF_LIB)
#   option(FFTWF_LIB "The full path to the fftw3f library (including the library)" )
    find_library(FFTWF_LIB fftw3f ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib
    find_library(FFTWF_THREADS_LIB fftw3f_threads ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib only if compiled with threads support

    if(FFTWF_LIB)
      set(FFTWF_FOUND 1)
      if(FFTWF_THREADS_LIB)
        set(FFTWF_LIB ${FFTWF_LIB} ${FFTWF_THREADS_LIB} )
      endif(FFTWF_THREADS_LIB)
    endif(FFTWF_LIB)
  endif(USE_FFTWF)

endif(USE_FFTWD OR USE_FFTWF)
