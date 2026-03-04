#
# Encapsulates building FFTW using FetchContent at configure time.
#
# NOTE: Internal building of FFTW is for convenience and testing.
#       The version built here uses conservative settings for broad hardware compatibility.
#       For production use where performance is critical, consider using ITK_USE_SYSTEM_FFTW
#       with an optimized FFTW installation.
#
include(ITK_CheckCCompilerFlag)
include(FetchContent)

# Find Threads package for threading support
find_package(Threads REQUIRED)

# FFTW limitation: can't be built in a directory with whitespace in its name
if(${CMAKE_CURRENT_BINARY_DIR} MATCHES ".*[ \t].*")
  message(
    FATAL_ERROR
    "Can't build FFTW in a directory with whitespace in its name"
  )
endif()

set(_fftw_target_version 3.3.10)
set(
  _fftw_url_hash
  "2d34b5ccac7b08740dbdacc6ebe451d8a34cf9d9bfec85a5e776e87adf94abfd803c222412d8e10fbaa4ed46f504aa87180396af1b108666cde4314a55610b40"
)
set(
  _fftw_url
  "https://data.kitware.com/api/v1/file/hashsum/sha512/${_fftw_url_hash}/download"
)

# Set policy CMP0077 to NEW so that option() respects normal variables
# This allows us to configure FFTW's build options without cache variables
set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)

# Build single precision FFTW if requested
if(ITK_USE_FFTWF)
  itk_download_attempt_check(FFTW)

  # Declare and fetch FFTW single precision
  FetchContent_Declare(
    fftw3f
    URL
      ${_fftw_url}
    URL_HASH SHA512=${_fftw_url_hash}
    DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
  )

  # Set FFTW options for single precision before populating
  # With CMP0077=NEW, option() respects normal variables
  set(BUILD_TESTS OFF)
  set(DISABLE_FORTRAN ON)
  set(ENABLE_AVX OFF)
  set(ENABLE_AVX2 OFF)
  set(ENABLE_FLOAT ON)
  set(ENABLE_LONG_DOUBLE OFF)
  set(ENABLE_OPENMP OFF)
  set(ENABLE_QUAD_PRECISION OFF)
  set(ENABLE_SSE OFF)
  set(ENABLE_SSE2 OFF)
  set(ENABLE_THREADS ON)
  set(WITH_COMBINED_THREADS OFF)

  FetchContent_MakeAvailable(fftw3f)
endif()

# Build double precision FFTW if requested
if(ITK_USE_FFTWD)
  if(NOT ITK_USE_FFTWF)
    itk_download_attempt_check(FFTW)
  endif()

  # Declare and fetch FFTW double precision

  # Only declare if not already declared for single precision
  FetchContent_Declare(
    fftw3
    URL
      ${_fftw_url}
    URL_HASH SHA512=${_fftw_url_hash}
    DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
    PATCH_COMMAND
      ${CMAKE_COMMAND} -E echo
      "Removing cmake_minimum_required from FFTW CMakeLists.txt" COMMAND
      ${CMAKE_COMMAND} -DINFILE=CMakeLists.txt -P
      "${ITKFFTW_SOURCE_DIR}/CMake/RemoveFirstLine.cmake"
  )

  # Set FFTW options for double precision
  # With CMP0077=NEW, option() respects normal variables
  set(BUILD_TESTS OFF)
  set(DISABLE_FORTRAN ON)
  set(ENABLE_AVX OFF)
  set(ENABLE_AVX2 OFF)
  set(ENABLE_FLOAT OFF)
  set(ENABLE_LONG_DOUBLE OFF)
  set(ENABLE_OPENMP OFF)
  set(ENABLE_QUAD_PRECISION OFF)
  set(ENABLE_SSE OFF)
  set(ENABLE_SSE2 OFF)
  set(ENABLE_THREADS ON)
  set(WITH_COMBINED_THREADS OFF)

  FetchContent_MakeAvailable(fftw3)
endif()
