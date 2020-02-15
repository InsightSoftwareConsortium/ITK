#
# Encapsulates building FFTW as an External Project.
include(ITK_CheckCCompilerFlag)


if(NOT ITK_USE_MKL AND NOT ITK_USE_CUFFTW)
  set(msg "ATTENTION: You have enabled the use of FFTW.")
  set(msg "${msg} This library is distributed under a GPL license.")
  set(msg "${msg} By enabling this option, the ITK libraries binary")
  set(msg "${msg} that is built will be covered by a GPL license")
  set(msg "${msg} and so will any executable that is linked against these libraries.")
  message("${msg}")
endif()

#--check_c_compiler_flag(-fopenmp C_HAS_fopenmp)
#--if(${C_HAS_fopenmp} AND FALSE)
#--    set(FFTW_THREADS_CONFIGURATION --enable-openmp)
#--    set(OPENMP_FLAG "-fopenmp")
#--  else()
    set(FFTW_THREADS_CONFIGURATION --enable-threads)
    set(OPENMP_FLAG "")
#--endif()

# from FFTW's configure command:
#--Some influential environment variables:
#--  CC          C compiler command
#--  CFLAGS      C compiler flags
#--  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
#--              nonstandard directory <lib dir>
#--  LIBS        libraries to pass to the linker, e.g. -l<library>
#--  CPPFLAGS    C/C++/Objective C preprocessor flags, e.g. -I<include dir> if
#--              you have headers in a nonstandard directory <include dir>

set(_additional_configure_env)
set(_additional_external_project_args)
set(_additional_deployment_target_flags)
if (APPLE)
  if(CMAKE_OSX_DEPLOYMENT_TARGET)
     set(_additional_deployment_target_flags "-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  endif()
  list(APPEND _additional_configure_env
        "SDKROOT=${CMAKE_OSX_SYSROOT}"
        "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
  )
  list(APPEND _additional_external_project_args
        BUILD_COMMAND
          env
            "SDKROOT=${CMAKE_OSX_SYSROOT}"
            "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
          make
        INSTALL_COMMAND
          env
            "SDKROOT=${CMAKE_OSX_SYSROOT}"
            "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
          make
            install
  )
endif()

## Perhaps in the future a set of TryCompiles could be used here.
set(FFTW_OPTIMIZATION_CONFIGURATION "" CACHE INTERNAL "architecture flags: --enable-sse --enable-sse2 --enable-altivec --enable-mips-ps --enable-cell")
if(ITK_USE_SYSTEM_FFTW)
  find_package( FFTW )
  link_directories(${FFTW_LIBDIR})
else()

  if(WIN32 AND NOT MINGW)
    message("Can't build fftw as external project on Windows")
    message(ERROR "install fftw and use ITK_USE_SYSTEM_FFTW")
  else()
    #
    # fftw limitation -- can't be built in
    # a directory with whitespace in its name.
    if(${CMAKE_CURRENT_BINARY_DIR} MATCHES ".*[ \t].*")
      message(FATAL_ERROR
        "Can't build fftw in a directory with whitespace in its name")
    endif()
    #
    # build fftw as an external project
    if(BUILD_SHARED_LIBS)
      set(FFTW_SHARED_FLAG --enable-shared)
    endif()
    #
    # set fPIC flag if needed
    set(GCC_POSITION_INDEPENDENT_CODE_FLAG "")
    if(CMAKE_POSITION_INDEPENDENT_CODE)
      set(GCC_POSITION_INDEPENDENT_CODE_FLAG "-fPIC")
    endif()

    set(_fftw_target_version 3.3.8)
    set(_fftw_url_hash "ab918b742a7c7dcb56390a0a0014f517a6dff9a2e4b4591060deeb2c652bf3c6868aa74559a422a276b853289b4b701bdcbd3d4d8c08943acf29167a7be81a38")
    set(_fftw_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_fftw_url_hash}/download")

    if(ITK_USE_FFTWF)
      itk_download_attempt_check(FFTW)
      ExternalProject_add(fftwf
        PREFIX fftwf-${_fftw_target_version}
        URL ${_fftw_url}
        URL_HASH SHA512=${_fftw_url_hash}
        DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
        CONFIGURE_COMMAND
          env
            "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
            "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} ${GCC_POSITION_INDEPENDENT_CODE_FLAG} ${_additional_deployment_target_flags}"
            "LDFLAGS=$ENV{LDFLAGS}"
            "LIBS=$ENV{LIBS}"
            "CPP=$ENV{CPP}"
            "CPPFLAGS=$ENV{CPPFLAGS} ${_additional_deployment_target_flags}"
            "CXXFLAGS=$ENV{CXXFLAGS} ${GCC_POSITION_INDEPENDENT_CODE_FLAG} ${_additional_deployment_target_flags}"
            ${_additional_configure_env}
          ${ITK_BINARY_DIR}/fftwf-${_fftw_target_version}/src/fftwf/configure
            ${FFTW_SHARED_FLAG}
            ${FFTW_OPTIMIZATION_CONFIGURATION}
            ${FFTW_THREADS_CONFIGURATION}
            --disable-fortran
            --enable-float
            --prefix=${ITK_BINARY_DIR}/fftw
        ${_additional_external_project_args}
        )
    endif()

    if(ITK_USE_FFTWD)
      itk_download_attempt_check(FFTW)
      ExternalProject_add(fftwd
        PREFIX fftwd-${_fftw_target_version}
        URL ${_fftw_url}
        URL_HASH SHA512=${_fftw_url_hash}
        DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
        CONFIGURE_COMMAND
          env
            "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
            "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} ${GCC_POSITION_INDEPENDENT_CODE_FLAG} ${_additional_deployment_target_flags}"
            "LDFLAGS=$ENV{LDFLAGS}"
            "LIBS=$ENV{LIBS}"
            "CPP=$ENV{CPP}"
            "CPPFLAGS=$ENV{CPPFLAGS} ${_additional_deployment_target_flags}"
            "CXXFLAGS=$ENV{CXXFLAGS} ${GCC_POSITION_INDEPENDENT_CODE_FLAG} ${_additional_deployment_target_flags}"
            ${_additional_configure_env}
          ${ITK_BINARY_DIR}/fftwd-${_fftw_target_version}/src/fftwd/configure
            ${FFTW_SHARED_FLAG}
            ${FFTW_OPTIMIZATION_CONFIGURATION}
            ${FFTW_THREADS_CONFIGURATION}
            --disable-fortran
            --disable-float
            --prefix=${ITK_BINARY_DIR}/fftw
        ${_additional_external_project_args}
        )
    endif()
    set(FFTW_INCLUDE_PATH ${ITK_BINARY_DIR}/fftw/include)
    set(FFTW_LIBDIR ${ITK_BINARY_DIR}/fftw/lib)
    link_directories(${FFTW_LIBDIR})
    include_directories(${FFTW_INCLUDE_PATH})
    #
    # copy libraries into install tree
    install(CODE
      "file(GLOB FFTW_LIBS ${ITK_BINARY_DIR}/fftw/lib/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_LIBS})" COMPONENT Development)
    #
    # copy headers into install tree
    install(CODE
      "file(GLOB FFTW_INC ${ITK_BINARY_DIR}/fftw/include/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_INC})" COMPONENT Development)

  endif()
endif()
