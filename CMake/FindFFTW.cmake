## FFTW can be compiled and subsequently linked against
## various data types.
## There is a single set of include files, and then multiple libraries,
## One for each type.  I.e. libfftw.a-->double, libfftwf.a-->float
##
## Usage:
##   find_package(FFTW REQUIRED COMPONENTS Double Float)
## Without COMPONENTS, the precisions enabled by `ITK_USE_FFTWD` and
## `ITK_USE_FFTWF` are searched for.
##
## Imported targets (carrying include directories and link dependencies):
##   FFTW::Double  -- double precision FFTW (with threads support if found)
##   FFTW::Float   -- single precision FFTW (with threads support if found)

## The following logic belongs in the individual package
## mark_as_advanced(ITK_USE_FFTWD)
## option(ITK_USE_FFTWD "Use double precision FFTW if found" ON)
## mark_as_advanced(ITK_USE_FFTWF)
## option(ITK_USE_FFTWF "Use single precision FFTW if found" ON)

## FFTW can be provided by the FFTW3 interface of the Intel oneAPI Math
## Kernel Library (oneMKL) 2021.3 or newer, through oneMKL's own CMake
## package (`MKL::MKL`). Both `ITK_USE_FFTWD` and `ITK_USE_FFTWF` will be
## turned ON. Unless already set, `MKL_LINK` is `static`, `MKL_INTERFACE`
## is `ilp64`, and `MKL_THREADING` is `sequential`.
##
## To use oneMKL, set `ITK_USE_MKL` to ON. The package is found from
## `MKL_DIR` (the directory holding `MKLConfig.cmake`), or from `MKLROOT`
## as a CMake or environment variable (the oneAPI `setvars` script sets it).
## oneMKL is 64-bit only and no longer supports macOS.

set(_fftw_double ${ITK_USE_FFTWD})
set(_fftw_float ${ITK_USE_FFTWF})
if(FFTW_FIND_COMPONENTS)
  set(_fftw_double OFF)
  set(_fftw_float OFF)
  foreach(_fftw_component IN LISTS FFTW_FIND_COMPONENTS)
    if(_fftw_component STREQUAL "Double")
      set(_fftw_double ON)
    elseif(_fftw_component STREQUAL "Float")
      set(_fftw_float ON)
    else()
      message(FATAL_ERROR "Unknown FFTW component: ${_fftw_component}")
    endif()
  endforeach()
endif()

if(_fftw_double OR _fftw_float)
  if(ITK_USE_MKL)
    if(NOT CMAKE_SIZEOF_VOID_P EQUAL 8)
      message(FATAL_ERROR "oneMKL supports only 64-bit targets.")
    endif()
    if(NOT DEFINED MKL_LINK)
      set(MKL_LINK static)
    endif()
    if(NOT DEFINED MKL_INTERFACE)
      set(MKL_INTERFACE ilp64)
    endif()
    if(NOT DEFINED MKL_THREADING)
      set(MKL_THREADING sequential)
    endif()
    find_package(
      MKL
      CONFIG
      REQUIRED
      HINTS
        "${MKLROOT}"
        "$ENV{MKLROOT}"
    )
  else()
    set(
      FFTW_INC_SEARCHPATH
      /sw/include
      /usr/include
      /usr/local/include
      /usr/include/fftw
      /usr/local/include/fftw
    )
  endif()

  if(ITK_USE_CUFFTW)
    find_path(CUFFTW_INCLUDE_PATH cufftw.h ${FFTW_INC_SEARCHPATH})
  elseif(NOT ITK_USE_MKL)
    find_path(FFTW_INCLUDE_PATH fftw3.h ${FFTW_INC_SEARCHPATH})
  endif()

  if(ITK_USE_MKL)
    # MKL::MKL carries <root>/include; the FFTW3 interface headers are in its fftw subdirectory.
    set(FFTW_INCLUDE "${MKL_ROOT}/include/fftw")
  elseif(FFTW_INCLUDE_PATH OR CUFFTW_INCLUDE_PATH)
    if(ITK_USE_CUFFTW)
      file(TO_CMAKE_PATH "${CUFFTW_INCLUDE_PATH}" CUFFTW_INCLUDE_PATH)
      set(FFTW_INCLUDE ${CUFFTW_INCLUDE_PATH})
    else()
      file(TO_CMAKE_PATH "${FFTW_INCLUDE_PATH}" FFTW_INCLUDE_PATH)
      set(FFTW_INCLUDE ${FFTW_INCLUDE_PATH})
    endif()
  endif()

  if(ITK_USE_CUFFTW)
    find_library(CUFFTW_LIB cufftw ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib
    find_library(CUFFT_LIB cufft ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib
    unset(FFTWD_LIBRARIES CACHE)
    unset(FFTWF_LIBRARIES CACHE)
    unset(FFTWD_THREADS_LIB CACHE)
    unset(FFTWF_THREADS_LIB CACHE)
    set(
      FFTWF_LIBRARIES
      ${CUFFT_LIB}
      ${CUFFTW_LIB}
    )
    set(
      FFTWD_LIBRARIES
      ${CUFFT_LIB}
      ${CUFFTW_LIB}
    )
  elseif(ITK_USE_MKL)
    set(FFTWD_LIBRARIES MKL::MKL)
    set(FFTWF_LIBRARIES MKL::MKL)
  else()
    get_filename_component(FFTW_INSTALL_BASE_PATH ${FFTW_INCLUDE_PATH} PATH)
    set(
      FFTW_LIB_SEARCHPATH
      ${FFTW_INSTALL_BASE_PATH}/lib
      ${FFTW_INSTALL_BASE_PATH}/lib64
      /usr/lib/fftw
      /usr/local/lib/fftw
    )
    unset(FFTWD_FOUND)
    if(_fftw_double)
      mark_as_advanced(
        FFTWD_BASE_LIB
        FFTWD_THREADS_LIB
        FFTWD_LIBRARIES
      )
      find_library(FFTWD_BASE_LIB fftw3 ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib
      find_library(FFTWD_THREADS_LIB fftw3_threads ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib only if compiled with threads support

      if(FFTWD_BASE_LIB)
        set(FFTWD_FOUND 1)
        get_filename_component(FFTW_LIBDIR ${FFTWD_BASE_LIB} PATH) #NOTE FFTWD_BASE_LIB and FFTWF_BASE_LIB must be installed in the same location
        set(FFTWD_LIBRARIES ${FFTWD_BASE_LIB})
        if(FFTWD_THREADS_LIB)
          set(
            FFTWD_LIBRARIES
            ${FFTWD_THREADS_LIB}
            ${FFTWD_BASE_LIB}
          )
        endif()
      else()
        message(FATAL_ERROR "fftw3 not found.")
      endif()
    endif()

    unset(FFTWF_FOUND)
    if(_fftw_float)
      mark_as_advanced(
        FFTWF_BASE_LIB
        FFTWF_THREADS_LIB
        FFTWF_LIBRARIES
      )
      find_library(FFTWF_BASE_LIB fftw3f ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib
      find_library(FFTWF_THREADS_LIB fftw3f_threads ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib only if compiled with threads support

      if(FFTWF_BASE_LIB)
        set(FFTWF_FOUND 1)
        get_filename_component(FFTW_LIBDIR ${FFTWF_BASE_LIB} PATH) #NOTE FFTWD_BASE_LIB and FFTWF_BASE_LIB must be installed in the same location
        set(FFTWF_LIBRARIES ${FFTWF_BASE_LIB})
        if(FFTWF_THREADS_LIB)
          set(
            FFTWF_LIBRARIES
            ${FFTWF_THREADS_LIB}
            ${FFTWF_BASE_LIB}
          )
        endif()
      else()
        message(FATAL_ERROR "fftw3f not found.")
      endif()
    endif()
  endif()
  set(_fftw_required_vars FFTW_INCLUDE)
  if(_fftw_double)
    list(APPEND _fftw_required_vars FFTWD_LIBRARIES)
  endif()
  if(_fftw_float)
    list(APPEND _fftw_required_vars FFTWF_LIBRARIES)
  endif()
  if(ITK_USE_CUFFTW)
    list(
      APPEND
      _fftw_required_vars
      CUFFT_LIB
      CUFFTW_LIB
    )
  endif()
  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(FFTW REQUIRED_VARS ${_fftw_required_vars})

  if(FFTW_FOUND)
    set(_fftw_extra_libraries "")
    if(NOT ITK_USE_MKL AND NOT ITK_USE_CUFFTW)
      find_package(Threads REQUIRED)
      list(APPEND _fftw_extra_libraries Threads::Threads)
      if(UNIX)
        list(APPEND _fftw_extra_libraries m)
      endif()
    endif()
    foreach(_fftw_precision IN ITEMS Double Float)
      string(SUBSTRING ${_fftw_precision} 0 1 _fftw_suffix)
      string(TOLOWER ${_fftw_precision} _fftw_selected)
      if(_fftw_${_fftw_selected} AND NOT TARGET FFTW::${_fftw_precision})
        add_library(FFTW::${_fftw_precision} INTERFACE IMPORTED)
        target_include_directories(
          FFTW::${_fftw_precision}
          SYSTEM
          INTERFACE
            "${FFTW_INCLUDE}"
        )
        target_link_libraries(
          FFTW::${_fftw_precision}
          INTERFACE
            ${FFTW${_fftw_suffix}_LIBRARIES}
            ${_fftw_extra_libraries}
        )
      endif()
    endforeach()
  endif()
endif()
