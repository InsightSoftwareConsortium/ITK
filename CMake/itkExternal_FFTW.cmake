#
# Encapsulates building FFTW as an External Project.

set(msg "ATTENTION: You have enabled the use of fftw,")
set(msg "${msg} this library is distributed under a GPL license.")
set(msg "${msg} By enabling this option, the binary of the ITK libraries")
set(msg "${msg} that you are going to build will be covered by a GPL license,")
set(msg "${msg} and so it will be any executable that you link against these libraries.")
message("${msg}")

## Perhaps in the future a set of TryCompiles could be used here.
set(FFTW_OPTIMIZATION_CONFIGURATION "" CACHE INTERNAL "architecture flags: --enable-sse --enable-sse2 --enable-altivec --enable-mips-ps --enable-cell")
if(USE_SYSTEM_FFTW)
  find_package( FFTW )
  link_directories(${FFTW_LIBDIR})
else(USE_SYSTEM_FFTW)
  if(WIN32 AND NOT MINGW)
    message("Can't build fftw as external project on Windows")
    message(ERROR "install fftw and use USE_SYSTEM_FFTW")
  else(WIN32 AND NOT MINGW)
    #
    # fftw limitation -- can't be built in
    # a directory with whitespace in its name.
    if(${CMAKE_CURRENT_BINARY_DIR} MATCHES ".*[ \t].*")
      message(FATAL_ERROR
        "Can't build fftw in a directory with whitespace in its name")
    endif(${CMAKE_CURRENT_BINARY_DIR} MATCHES ".*[ \t].*")
    #
    # build fftw as an external project
    if(BUILD_SHARED_LIBS)
      set(FFTW_SHARED_FLAG --enable-shared)
    endif(BUILD_SHARED_LIBS)
    if(USE_FFTWF)
      ExternalProject_add(fftwf
        PREFIX fftwf
        URL "http://www.fftw.org/fftw-3.2.2.tar.gz"
        URL_MD5 b616e5c91218cc778b5aa735fefb61ae
        CONFIGURE_COMMAND ${ITK_BINARY_DIR}/fftwf/src/fftwf/configure
        ${FFTW_SHARED_FLAG}
        ${FFTW_OPTIMIZATION_CONFIGURATION}
        --enable-float
        --enable-threads
        --prefix=${ITK_BINARY_DIR}/fftw
        )
    endif(USE_FFTWF)

    if(USE_FFTWD)
      ExternalProject_add(fftwd
        PREFIX fftwd
        URL "http://www.fftw.org/fftw-3.2.2.tar.gz"
        URL_MD5 b616e5c91218cc778b5aa735fefb61ae
        CONFIGURE_COMMAND ${ITK_BINARY_DIR}/fftwd/src/fftwd/configure
        ${FFTW_SHARED_FLAG}
        ${FFTW_OPTIMIZATION_CONFIGURATION}
        --enable-threads
        --prefix=${ITK_BINARY_DIR}/fftw
        )
    endif(USE_FFTWD)
    link_directories(${ITK_BINARY_DIR}/fftw/lib)
    # backwards compatibility
    set(FFTW_INCLUDE_PATH ${ITK_BINARY_DIR}/fftw/include)
    #
    # copy libraries into install tree
    install(CODE
      "file(GLOB FFTW_LIBS ${ITK_BINARY_DIR}/fftw/lib/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_LIBS})")
    #
    # copy headers into install tree
    install(CODE
      "file(GLOB FFTW_INC ${ITK_BINARY_DIR}/fftw/include/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/include/InsightToolkit-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Algorithms\"
TYPE FILE FILES \${FFTW_INC})")

  endif(WIN32 AND NOT MINGW)
endif(USE_SYSTEM_FFTW)
