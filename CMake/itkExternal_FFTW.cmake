#
# Encapsulates building FFTW as an External Project.
include(ITK_CheckCCompilerFlag)


set(msg "ATTENTION: You have enabled the use of FFTW.")
set(msg "${msg} This library is distributed under a GPL license.")
set(msg "${msg} By enabling this option, the ITK libraries binary")
set(msg "${msg} that is built will be covered by a GPL license")
set(msg "${msg} and so will any executable that is linked against these libraries.")
message("${msg}")

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
if (APPLE)
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

    set(_fftw_target_version 3.3.4)
    set(_fftw_url_md5 "2edab8c06b24feeb3b82bbb3ebf3e7b3")
    set(_fftw_url "https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=${_fftw_url_md5}&name=fftw-${_fftw_target_version}.tar.gz")

    if(ITK_USE_FFTWF)
      itk_download_attempt_check(FFTW)
      ExternalProject_add(fftwf
        PREFIX fftwf
        URL "${_fftw_url}"
        URL_MD5 "${_fftw_url_md5}"
        DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
        CONFIGURE_COMMAND
          env
            "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
            "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} ${GCC_POSITION_INDEPENDENT_CODE_FLAG}"
            "LDFLAGS=$ENV{LDFLAGS}"
            "LIBS=$ENV{LIBS}"
            "CPP=$ENV{CPP}"
            "CPPFLAGS=$ENV{CPPFLAGS}"
            "CXXFLAGS=$ENV{CXXFLAGS} ${GCC_POSITION_INDEPENDENT_CODE_FLAG}"
            ${_additional_configure_env}
          ${ITK_BINARY_DIR}/fftwf/src/fftwf/configure
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
        PREFIX fftwd
        URL "${_fftw_url}"
        URL_MD5 "${_fftw_url_md5}"
        DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
        CONFIGURE_COMMAND
          env
           "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
           "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} ${GCC_POSITION_INDEPENDENT_CODE_FLAG}"
           "LDFLAGS=$ENV{LDFLAGS}"
           "LIBS=$ENV{LIBS}"
           "CPP=$ENV{CPP}"
           "CPPFLAGS=$ENV{CPPFLAGS}"
           "CXXFLAGS=$ENV{CXXFLAGS} ${GCC_POSITION_INDEPENDENT_CODE_FLAG}"
            ${_additional_configure_env}
          ${ITK_BINARY_DIR}/fftwd/src/fftwd/configure
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
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Algorithms\"
TYPE FILE FILES \${FFTW_INC})" COMPONENT Development)

  endif()
endif()
