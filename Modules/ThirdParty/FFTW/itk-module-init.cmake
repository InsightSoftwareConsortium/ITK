# ITK_USE_SYSTEM_FFTW -- locate a ready-built FFTW installation
option(
  ITK_USE_SYSTEM_FFTW
  "Use an installed version of FFTW"
  ${ITK_USE_SYSTEM_LIBRARIES}
)
mark_as_advanced(ITK_USE_SYSTEM_FFTW)

option(ITK_USE_FFTWD "Use double precision FFTW if found" OFF)
mark_as_advanced(ITK_USE_FFTWD)
option(ITK_USE_FFTWF "Use single precision FFTW if found" OFF)
mark_as_advanced(ITK_USE_FFTWF)

if(ITK_USE_SYSTEM_FFTW)
  set(_FFTW3_required_components "")
  if(ITK_USE_FFTWD)
    list(
      APPEND
      _FFTW3_required_components
      fftw3
      fftw3_threads
    )
  endif()
  if(ITK_USE_FFTWF)
    list(
      APPEND
      _FFTW3_required_components
      fftw3f
      fftw3f_threads
    )
  endif()

  list(PREPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/CMake")
  find_package(FFTW3 REQUIRED COMPONENTS ${_FFTW3_required_components})
  list(REMOVE_AT CMAKE_MODULE_PATH 0)
endif()
