function(itk_fftw_add_precision precision)
  set(CMAKE_POLICY_VERSION_MINIMUM ${ITK_OLDEST_VALIDATED_POLICIES_VERSION})
  set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
  set(BUILD_TESTS OFF)
  set(DISABLE_FORTRAN ON)
  set(ENABLE_THREADS ON)
  set(WITH_COMBINED_THREADS OFF)
  set(ENABLE_OPENMP OFF)
  set(ENABLE_LONG_DOUBLE OFF)
  set(ENABLE_QUAD_PRECISION OFF)
  set(ENABLE_FLOAT OFF)
  set(ENABLE_SSE OFF)
  if(precision STREQUAL "float")
    set(ENABLE_FLOAT ON)
    set(ENABLE_SSE ${FFTW_ENABLE_SSE})
    set(_fftw_library fftw3f)
  else()
    set(_fftw_library fftw3)
  endif()
  foreach(isa IN ITEMS NEON SSE2 AVX AVX2)
    set(ENABLE_${isa} ${FFTW_ENABLE_${isa}})
  endforeach()
  # Mask cached SIMD probes when a codelet option is disabled.
  foreach(
    isa
    IN
    ITEMS
      SSE
      SSE2
      AVX
      AVX2
      FMA
  )
    set(HAVE_${isa} 0)
  endforeach()

  add_subdirectory("${fftw_SOURCE_DIR}" "${precision}" EXCLUDE_FROM_ALL)

  foreach(_target IN ITEMS ${_fftw_library} ${_fftw_library}_threads)
    set_property(
      TARGET
        ${_target}
      PROPERTY
        INTERFACE_INCLUDE_DIRECTORIES
          "${ITKFFTW_GENEX_INCLUDE_DIRS}"
    )
    itk_module_target(${_target})
  endforeach()
endfunction()

get_property(_fftw_precisions GLOBAL PROPERTY ITKFFTW_PRECISIONS)
foreach(_precision IN LISTS _fftw_precisions)
  itk_fftw_add_precision(${_precision})
endforeach()

install(
  FILES
    "${fftw_SOURCE_DIR}/api/fftw3.h"
  DESTINATION ${ITKFFTW_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
