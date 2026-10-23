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

## FFTW can be included from Intel MKL library (static). Both
## `ITK_USE_FFTWD` and `ITK_USE_FFTWF` will be turned ON.
##
## To use the MKL implementation, set `ITK_USE_MKL` to ON and set
##  `MKLROOT` to the installation directory of the MKL library on
## your system and point to the `mkl` subdirectory:
## ** On Linux, the default install directory is:
## /opt/intel/compilers_and_libraries/linux/
## ** On Windows, it is:
## `C:\Program Files (x86)\Intel\oneAPI\mkl\2021.4.0`
## ** On Mac, it is:
## `/opt/intel/compilers_and_libraries/mac/mkl`
##
## '''Disabled''': If the compiler supports it, `ITK_USE_TBB_WITH_MKL` is automatically
## set to ON and MKL is linked against `TBB`. Otherwise it is linked against
## `mkl_sequential`.
##
## Note: if the environment variable `MKLROOT` is set, its value will be used
## to set the initial value of the CMake variable `MKLROOT` (see
## https://software.intel.com/en-us/mkl-linux-developer-guide-scripts-to-set-environment-variables).

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
    # If the user has provided the MKL include path then search nearby for library files
    if(FFTW_INCLUDE_PATH)
      # mkl/<version>/include/fftw -> mkl/<version>
      get_filename_component(MKLROOT ${FFTW_INCLUDE_PATH} DIRECTORY)
      get_filename_component(MKLROOT ${MKLROOT} DIRECTORY)
    elseif(DEFINED ENV{MKLROOT})
      set(MKLROOT_default $ENV{MKLROOT})
    elseif(WIN32)
      set(MKLROOT_default "C:/Program Files (x86)/Intel/oneAPI/mkl/2021.4.0")
    elseif(APPLE)
      set(MKLROOT_default "/opt/intel/compilers_and_libraries/mac/mkl")
    elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
      set(MKLROOT_default "/opt/intel/compilers_and_libraries/linux/mkl")
    else()
      message(FATAL_ERROR "System not supported for MKL.")
    endif()
    set(MKLROOT ${MKLROOT_default} CACHE PATH "Intel path containing MKL")
    set(FFTW_INC_SEARCHPATH ${MKLROOT}/include/fftw)
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
  else()
    find_path(FFTW_INCLUDE_PATH fftw3.h ${FFTW_INC_SEARCHPATH})
  endif()

  if(FFTW_INCLUDE_PATH OR CUFFTW_INCLUDE_PATH)
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
    # '''Disabled''': `ITK_USE_TBB_WITH_MKL`. To configure MKL with TBB, the TBB library
    # has to be found. This is not currently taken care of as part of this file and may
    # result in conflicts if `Module_ITKTBB` is ON.
    #    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    #      # See MKL configuration help page:
    #      # https://software.intel.com/en-us/articles/intel-mkl-link-line-advisor
    #      message("TBB threading option not available with Clang compiler.")
    #      set(ITK_USE_TBB_WITH_MKL OFF CACHE BOOL "Use TBB threading in MKL" FORCE)
    #    else()
    #      option(ITK_USE_TBB_WITH_MKL "Use TBB threading in MKL" ON)
    #      mark_as_advanced(ITK_USE_TBB_WITH_MKL)
    #    endif()

    if(CMAKE_SIZEOF_VOID_P EQUAL "8")
      if(APPLE)
        set(FFTW_LIB_SEARCHPATH ${MKLROOT}/lib)
      else()
        set(FFTW_LIB_SEARCHPATH ${MKLROOT}/lib/intel64)
      endif()
      set(MKL_LIBRARY mkl_intel_ilp64)
      if(WIN32)
        set(MKL_OPTIONS /DMKL_ILP64)
      else()
        set(
          MKL_OPTIONS
          -DMKL_ILP64
          -m64
        )
      endif()
    else()
      set(FFTW_LIB_SEARCHPATH ${MKLROOT}/lib/ia32)
      if(WIN)
        set(MKL_LIBRARY mkl_intel_c)
      else()
        set(MKL_LIBRARY mkl_intel)
      endif()
    endif()
    set(MKL_EXTRA_LIBRARIES mkl_core)
    # Force to sequential for now.
    list(APPEND MKL_EXTRA_LIBRARIES mkl_sequential)
    #    if(ITK_USE_TBB_WITH_MKL)
    #      list(APPEND MKL_EXTRA_LIBRARIES mkl_tbb_thread)
    #    else()
    #        list(APPEND MKL_EXTRA_LIBRARIES mkl_sequential)
    #    endif()
    set(
      FFTW_LIBRARY_NAMES
      ${MKL_LIBRARY}
      ${MKL_EXTRA_LIBRARIES}
    )

    macro(FFTWD_LIBRARIES_START)
      unset(FFTWD_LIBRARIES CACHE)
      unset(FFTWF_LIBRARIES CACHE)
      unset(FFTWD_THREADS_LIB CACHE)
      unset(FFTWF_THREADS_LIB CACHE)
      if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
        set(FFTWD_LIBRARIES -Wl,--start-group)
      endif()
    endmacro()

    macro(FFTWD_LIBRARIES_END)
      if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
        list(
          APPEND
          FFTWD_LIBRARIES
          -Wl,--end-group
          -lpthread
          -lm
          -ldl
        )
        # Force to sequential for now.
        #        if(ITK_USE_TBB_WITH_MKL)
        #          list(APPEND FFTWD_LIBRARIES -ltbb -lstdc++)
        #        endif()
      endif()
    endmacro()

    # Because of circular dependencies between the libraries, we need to use
    # --start-group and --end-group on UNIX.
    fftwd_libraries_start()
    foreach(LIB ${FFTW_LIBRARY_NAMES})
      string(TOUPPER ${LIB} LIB_UPPER)
      mark_as_advanced(${LIB_UPPER}_LIB)
      set(
        LIB_NAME
        ${CMAKE_STATIC_LIBRARY_PREFIX}${LIB}${CMAKE_STATIC_LIBRARY_SUFFIX}
      )
      find_library(${LIB_UPPER}_LIB ${LIB_NAME} ${FFTW_LIB_SEARCHPATH})
      if(${LIB_UPPER}_LIB)
        set(${LIB}_FOUND 1)
        list(APPEND FFTWD_LIBRARIES ${${LIB_UPPER}_LIB})
      else()
        message(FATAL_ERROR "${LIB_NAME} not found.")
      endif()
    endforeach()
    fftwd_libraries_end()
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
  if(ITK_USE_MKL)
    set(FFTWF_LIBRARIES ${FFTWD_LIBRARIES})
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
  elseif(NOT ITK_USE_MKL)
    if(_fftw_double)
      list(APPEND _fftw_required_vars FFTWD_THREADS_LIB)
    endif()
    if(_fftw_float)
      list(APPEND _fftw_required_vars FFTWF_THREADS_LIB)
    endif()
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
        if(ITK_USE_MKL)
          target_compile_options(
            FFTW::${_fftw_precision}
            INTERFACE
              ${MKL_OPTIONS}
          )
        endif()
      endif()
    endforeach()
  endif()
endif()
