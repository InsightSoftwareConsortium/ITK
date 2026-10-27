project(ITKFFTW)
set(ITKFFTW_THIRD_PARTY 1)
set(ITKFFTW_LIBRARIES "")

set(_fftw_precisions "")
set(_fftw_components "")
set(_fftw_library_names "")
if(ITK_USE_FFTWD)
  list(APPEND _fftw_precisions double)
  list(APPEND _fftw_components Double)
  list(APPEND _fftw_library_names fftw3)
endif()
if(ITK_USE_FFTWF)
  list(APPEND _fftw_precisions float)
  list(APPEND _fftw_components Float)
  list(APPEND _fftw_library_names fftw3f)
endif()

if(NOT _fftw_precisions)
  message(
    FATAL_ERROR
    "Module_ITKFFTW requires ITK_USE_FFTWD or ITK_USE_FFTWF to be ON."
  )
endif()

if(NOT ITK_USE_MKL AND NOT ITK_USE_CUFFTW)
  message(
    STATUS
    "FFTW is GPL licensed. Linking ITK with FFTW subjects the resulting binaries to the GPL."
  )
endif()

if(ITK_USE_SYSTEM_FFTW)
  list(
    TRANSFORM _fftw_components
    PREPEND
      "FFTW::"
    OUTPUT_VARIABLE ITKFFTW_LIBRARIES
  )
  find_package(FFTW REQUIRED COMPONENTS ${_fftw_components})
  set(ITKFFTW_NO_SRC 1)

  # Seed the app's cache with what ITK found, unless the app provides its own root.
  set(_fftw_root_var FFTW_ROOT)
  set(_fftw_hint_vars "")
  if(ITK_USE_MKL)
    set(_fftw_root_var MKLROOT)
    set(_fftw_hint_vars MKLROOT)
  elseif(NOT ITK_USE_CUFFTW)
    set(
      _fftw_hint_vars
      FFTW_INCLUDE_PATH
      FFTWD_BASE_LIB
      FFTWD_THREADS_LIB
      FFTWF_BASE_LIB
      FFTWF_THREADS_LIB
    )
  endif()
  set(_fftw_hint_code "")
  foreach(_fftw_var IN LISTS _fftw_hint_vars)
    if(${_fftw_var})
      set(_fftw_type FILEPATH)
      if(_fftw_var MATCHES "^(FFTW_INCLUDE_PATH|MKLROOT)$")
        set(_fftw_type PATH)
      endif()
      string(
        APPEND
        _fftw_hint_code
        "
  set(${_fftw_var} \"${${_fftw_var}}\" CACHE ${_fftw_type} \"\")"
      )
    endif()
  endforeach()
  if(_fftw_hint_code)
    set(
      _fftw_hint_code
      "
if(NOT DEFINED ${_fftw_root_var} AND NOT DEFINED ENV{${_fftw_root_var}})${_fftw_hint_code}
endif()"
    )
  endif()

  # When this module is loaded by an app, find FFTW with ITK's finder too.
  set(
    _fftw_find_code
    "
set(ITK_USE_MKL ${ITK_USE_MKL})
set(ITK_USE_CUFFTW ${ITK_USE_CUFFTW})${_fftw_hint_code}
set(_itk_fftw_module_path \"\${CMAKE_MODULE_PATH}\")
list(PREPEND CMAKE_MODULE_PATH \"\${ITK_CMAKE_DIR}\")
find_package(FFTW REQUIRED COMPONENTS ${_fftw_components})
set(CMAKE_MODULE_PATH \"\${_itk_fftw_module_path}\")
unset(_itk_fftw_module_path)
"
  )
  set(ITKFFTW_EXPORT_CODE_INSTALL "${_fftw_find_code}")
  set(
    ITKFFTW_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)${_fftw_find_code}endif()
"
  )
else()
  include(FetchContent)
  include(itkFFTWOptions.cmake)
  set(_fftw_version 3.3.10)
  set(
    _fftw_url_hash
    "2d34b5ccac7b08740dbdacc6ebe451d8a34cf9d9bfec85a5e776e87adf94abfd803c222412d8e10fbaa4ed46f504aa87180396af1b108666cde4314a55610b40"
  )
  # FFTW configures one precision per directory; populate without adding it.
  FetchContent_Declare(
    FFTW
    URL
      "https://data.kitware.com/api/v1/file/hashsum/sha512/${_fftw_url_hash}/download"
    URL_HASH SHA512=${_fftw_url_hash}
    DOWNLOAD_NAME fftw-${_fftw_version}.tar.gz
    SOURCE_SUBDIR
    itk-fetch-only
  )
  if(NOT FETCHCONTENT_SOURCE_DIR_FFTW AND NOT FETCHCONTENT_FULLY_DISCONNECTED)
    itk_download_attempt_check(FFTW)
  endif()
  FetchContent_MakeAvailable(FFTW)
  # Expose only the public header, not FFTW's internal api/ headers.
  configure_file(
    "${fftw_SOURCE_DIR}/api/fftw3.h"
    "${ITKFFTW_BINARY_DIR}/include/fftw3.h"
    COPYONLY
  )
  set(ITKFFTW_INCLUDE_DIRS "${ITKFFTW_BINARY_DIR}/include")
  # Namespaced names keep ITK from exporting bare fftw3* compatibility targets.
  set(_fftw_ns "${ITK_MODULE_ITKFFTW_TARGETS_NAMESPACE}")
  foreach(_fftw_library IN LISTS _fftw_library_names)
    list(
      APPEND
      ITKFFTW_LIBRARIES
      ${_fftw_ns}${_fftw_library}_threads
      ${_fftw_ns}${_fftw_library}
    )
  endforeach()
endif()

itk_module_impl()

# The exported module target references this directory even without headers.
install(
  DIRECTORY
  DESTINATION ${ITKFFTW_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
