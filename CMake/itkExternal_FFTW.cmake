#
# Encapsulates building FFTW as an External Project.
#
# NOTE: internal building of fftw is for convenience,
#       and the version of fftw built here does not
#       use modern hardware optimzations.
#
#       The build configuration choosen to be
#       generalizable to as many hardware platforms.
#       Being backward compatible for decades
#       old hardware is the goal of this internal
#       representation.
#
#       This is primarily used to support testing
#       and should not be used for production
#       builds where performance is a concern.
#
# These instructions follow the guidance provided for modern cmake usage as described:
# https://github.com/dev-cafe/cmake-cookbook/blob/master/chapter-08/recipe-03/c-example/external/upstream/fftw3/CMakeLists.txt
#
include(ITK_CheckCCompilerFlag)


if(NOT ITK_USE_MKL AND NOT ITK_USE_CUFFTW)
  set(msg "ATTENTION: You have enabled the use of FFTW.")
  set(msg "${msg} This library is distributed under a GPL license.")
  set(msg "${msg} By enabling this option, the ITK libraries binary")
  set(msg "${msg} that is built will be covered by a GPL license")
  set(msg "${msg} and so will any executable that is linked against these libraries.")
  message("${msg}")
endif()


if(NOT ITK_USE_SYSTEM_FFTW)
    #
    # fftw limitation -- can't be built in
    # a directory with whitespace in its name.
    if(${CMAKE_CURRENT_BINARY_DIR} MATCHES ".*[ \t].*")
      message(FATAL_ERROR
        "Can't build fftw in a directory with whitespace in its name")
    endif()

    if(WIN32)
      # on windows the build of FFTW must match that of ITK
      set(FFTW_BUILD_TYPE ${CMAKE_BUILD_TYPE})
    else()
      set(FFTW_BUILD_TYPE Release)
    endif()

    include(GNUInstallDirs)

    set(_fftw_target_version 3.3.8)
    set(_fftw_url_hash "ab918b742a7c7dcb56390a0a0014f517a6dff9a2e4b4591060deeb2c652bf3c6868aa74559a422a276b853289b4b701bdcbd3d4d8c08943acf29167a7be81a38")
    set(_fftw_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_fftw_url_hash}/download")

    set(FFTW_STAGED_INSTALL_PREFIX "${ITK_BINARY_DIR}/fftw")
    set(PROJ_FFTWD_DEPENDS "")
    if(ITK_USE_FFTWF)
      itk_download_attempt_check(FFTW)
      ExternalProject_add(fftwf
        PREFIX fftwf-${_fftw_target_version}
        INSTALL_DIR ${FFTW_STAGED_INSTALL_PREFIX}
        URL ${_fftw_url}
        URL_HASH SHA512=${_fftw_url_hash}
        DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
        DOWNLOAD_NO_PROGRESS 1
        UPDATE_COMMAND ""
        LOG_CONFIGURE 1
        LOG_BUILD 1
        LOG_INSTALL 1
        CMAKE_CACHE_ARGS
           -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
           -DBUILD_TESTS:BOOL=OFF
           -DCMAKE_BUILD_TYPE:STRING=${FFTW_BUILD_TYPE}
           -DCMAKE_INSTALL_PREFIX:PATH=${FFTW_STAGED_INSTALL_PREFIX}
           -DDISABLE_FORTRAN:BOOL=ON
           -DENABLE_AVX:BOOL=OFF
           -DENABLE_AVX2:BOOL=OFF
           -DENABLE_FLOAT:BOOL=ON
           -DENABLE_LONG_DOUBLE:BOOL=OFF
           -DENABLE_OPENMP:BOOL=OFF
           -DENABLE_QUAD_PRECISION:BOOL=OFF
           -DENABLE_SSE:BOOL=OFF
           -DENABLE_SSE2:BOOL=OFF
           -DENABLE_THREADS:BOOL=ON
           -DCMAKE_APPLE_SILICON_PROCESSOR:STRING=${CMAKE_APPLE_SILICON_PROCESSOR}
           -DCMAKE_C_COMPILER_LAUNCHER:PATH=${CMAKE_C_COMPILER_LAUNCHER}
           -DCMAKE_C_COMPILER:PATH=${CMAKE_C_COMPILER}
           -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
           -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT}
           -DCMAKE_OSX_DEPLOYMENT_TARGET:PATH=${CMAKE_OSX_DEPLOYMENT_TARGET}
           -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=${CMAKE_POSITION_INDEPENDENT_CODE}
        )
      # set(
      #   FFTW3f_DIR ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/cmake/fftw3f
      #   CACHE PATH "Path to internally built single precision FFTW3Config.cmake"
      #   FORCE
      #  )
      # Can not find package, it does not yet exist find_package(FFTW3f CONFIG REQUIRED)
      # but we know where it will eventually be!
      set(FFTW3f_INCLUDE_DIRS ${FFTW_STAGED_INSTALL_PREFIX}/include )
      set(FFTW3f_LIBRARY_DIRS ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR})
      set(FFTW_INCLUDE ${FFTW3_INCLUDE_DIRS})
      set(FFTW_LIBDIR  ${FFTW3_LIBRARY_DIRS})
      set(ITK_FFTWF_LIBRARIES_NAMES fftw3f_threads fftw3f)
      set(PROJ_FFTWD_DEPENDS "fftwf")
    endif()

    if(ITK_USE_FFTWD)
      itk_download_attempt_check(FFTW)
      ExternalProject_add(fftwd
        PREFIX fftwd-${_fftw_target_version}
        INSTALL_DIR ${FFTW_STAGED_INSTALL_PREFIX}
        URL ${_fftw_url}
        URL_HASH SHA512=${_fftw_url_hash}
        DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
        DOWNLOAD_NO_PROGRESS 1
        UPDATE_COMMAND ""
        LOG_CONFIGURE 1
        LOG_BUILD 1
        LOG_INSTALL 1
        CMAKE_CACHE_ARGS
           -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
           -DBUILD_TESTS:BOOL=OFF
           -DCMAKE_BUILD_TYPE:STRING=${FFTW_BUILD_TYPE}
           -DCMAKE_INSTALL_PREFIX:PATH=${FFTW_STAGED_INSTALL_PREFIX}
           -DDISABLE_FORTRAN:BOOL=ON
           -DENABLE_AVX:BOOL=OFF
           -DENABLE_AVX2:BOOL=OFF
           -DENABLE_FLOAT:BOOL=OFF
           -DENABLE_LONG_DOUBLE:BOOL=OFF
           -DENABLE_OPENMP:BOOL=OFF
           -DENABLE_QUAD_PRECISION:BOOL=OFF
           -DENABLE_SSE:BOOL=OFF
           -DENABLE_SSE2:BOOL=OFF
           -DENABLE_THREADS:BOOL=ON
           -DCMAKE_APPLE_SILICON_PROCESSOR:STRING=${CMAKE_APPLE_SILICON_PROCESSOR}
           -DCMAKE_C_COMPILER_LAUNCHER:PATH=${CMAKE_C_COMPILER_LAUNCHER}
           -DCMAKE_C_COMPILER:PATH=${CMAKE_C_COMPILER}
           -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
           -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT}
           -DCMAKE_OSX_DEPLOYMENT_TARGET:PATH=${CMAKE_OSX_DEPLOYMENT_TARGET}
           -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=${CMAKE_POSITION_INDEPENDENT_CODE}
        DEPENDS ${PROJ_FFTWD_DEPENDS} # Avoid potential collisions on install
        )
      # set(
      #   FFTW3_DIR ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/cmake/fftw3
      #   CACHE PATH "Path to internally built double precision FFTW3Config.cmake"
      #   FORCE
      #  )
      # Can not find package, it does not yet exist find_package(FFTW3 CONFIG REQUIRED)
      set(FFTW3_INCLUDE_DIRS ${FFTW_STAGED_INSTALL_PREFIX}/include )
      set(FFTW3_LIBRARY_DIRS ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR})

      set(FFTW_INCLUDE ${FFTW3_INCLUDE_DIRS})
      set(FFTW_LIBDIR  ${FFTW3_LIBRARY_DIRS})
      set(ITK_FFTWD_LIBRARIES_NAMES fftw3_threads fftw3)
    endif()

    #
    # copy libraries into install tree, NOTE: DESTINATION MUST EXACTLY MATCH values from main CMakeLists.txt for FFTW_LIBDIR
    install(CODE
      "file(GLOB FFTW_LIBS ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_LIBS})"
      COMPONENT Development)
    #
    # copy headers into install tree
    install(CODE
      "file(GLOB FFTW_INC ${FFTW_STAGED_INSTALL_PREFIX}/include/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_INC})"
      COMPONENT Development)
else()
  #Search the filesystem for compatible versions
  find_package( FFTW ) # Use local itk FindFFTW.config to set variables consistently both with/without USE_SYSTEM_FFTW
endif()
