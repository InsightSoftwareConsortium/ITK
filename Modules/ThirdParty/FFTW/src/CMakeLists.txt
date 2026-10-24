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

  # FFTW is always built optimized, whatever the ITK build type; other flags (e.g. -g, sanitizers) are kept.
  # Optimization tokens are removed in place so quoted arguments are left intact.
  set(_fftw_opt_flag " [-/](O[0-9sgdxz]?|Ob[0-9]|RTC[1csu]*) ")
  foreach(_config IN ITEMS DEBUG RELWITHDEBINFO MINSIZEREL)
    set(_fftw_flags " ${CMAKE_C_FLAGS_${_config}} ")
    while(_fftw_flags MATCHES "${_fftw_opt_flag}")
      string(REGEX REPLACE "${_fftw_opt_flag}" " " _fftw_flags "${_fftw_flags}")
    endwhile()
    string(STRIP "${_fftw_flags}" _fftw_flags)
    set(CMAKE_C_FLAGS_${_config} "${_fftw_flags} ${CMAKE_C_FLAGS_RELEASE}")
  endforeach()
  get_property(_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
  if(NOT _multi_config AND NOT CMAKE_BUILD_TYPE)
    string(APPEND CMAKE_C_FLAGS " ${CMAKE_C_FLAGS_RELEASE}")
  endif()

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

foreach(_precision IN LISTS _fftw_precisions)
  itk_fftw_add_precision(${_precision})
endforeach()

install(
  FILES
    "${ITKFFTW_BINARY_DIR}/include/fftw3.h"
  DESTINATION ${ITKFFTW_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
