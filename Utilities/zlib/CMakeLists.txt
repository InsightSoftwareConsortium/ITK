PROJECT(ITKZLIB)
INCLUDE_REGULAR_EXPRESSION("^(deflate|inf|trees|zconf|zlib|zutil).*$")

INCLUDE_DIRECTORIES(${ITKZLIB_SOURCE_DIR})
INCLUDE_DIRECTORIES(${ITKZLIB_BINARY_DIR})

# source files for zlib
SET(ZLIB_SRCS
adler32.c   gzio.c      inftrees.c  uncompr.c
compress.c  infblock.c  infutil.c   zutil.c
crc32.c     infcodes.c  
deflate.c   inffast.c   
inflate.c   trees.c 
)

# for windows add the .def and .rc files to the source list
IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    SET(ZLIB_DLL 1)
  ENDIF(BUILD_SHARED_LIBS)
  IF(NOT UNIX)
    IF(NOT BORLAND)
       SET(ZLIB_SRCS ${ZLIB_SRCS} zlib.def zlib.rc  )
    ENDIF(NOT BORLAND)  
  ENDIF(NOT UNIX)
ENDIF(WIN32)

CONFIGURE_FILE(${ITKZLIB_SOURCE_DIR}/.NoDartCoverage 
               ${ITKZLIB_BINARY_DIR}/.NoDartCoverage)
CONFIGURE_FILE(${ITKZLIB_SOURCE_DIR}/zlibDllConfig.h.in
               ${ITKZLIB_BINARY_DIR}/zlibDllConfig.h)


ADD_LIBRARY(itkzlib ${ZLIB_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit itkzlib)
INSTALL_FILES(/include/InsightToolkit/Utilities/zlib .h
  zlib zconf zlibDllConfig
)
