PROJECT(ITKPNG)
INCLUDE_REGULAR_EXPRESSION("^png.*$")

INCLUDE_DIRECTORIES(${ITKPNG_SOURCE_DIR})
INCLUDE_DIRECTORIES(${ITKPNG_BINARY_DIR})

# source files for png
SET(PNG_SRCS
pngget.c    pngrio.c    pngwrite.c
png.c       pngmem.c    pngrtran.c  pngtrans.c  pngwtran.c
pngerror.c  pngpread.c  pngrutil.c  pngvcrd.c   pngwutil.c
pnggccrd.c  pngread.c   pngset.c   pngwio.c
)

IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DPNG_BUILD_DLL)
    SET(PNG_NO_MODULEDEF 1)
  ELSE(BUILD_SHARED_LIBS)
    SET(PNG_STATIC 1)
  ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

CONFIGURE_FILE(${ITKPNG_SOURCE_DIR}/.NoDartCoverage
               ${ITKPNG_BINARY_DIR}/.NoDartCoverage)
CONFIGURE_FILE(${ITKPNG_SOURCE_DIR}/pngDllConfig.h.in
               ${ITKPNG_BINARY_DIR}/pngDllConfig.h)

ADD_LIBRARY(itkpng ${PNG_SRCS})


IF(UNIX)
TARGET_LINK_LIBRARIES(itkpng ${ITK_ZLIB_LIBRARIES} -lm)
ELSE(UNIX)
TARGET_LINK_LIBRARIES(itkpng ${ITK_ZLIB_LIBRARIES})
ENDIF(UNIX)

INSTALL_TARGETS(/lib/InsightToolkit itkpng)
INSTALL_FILES(/include/InsightToolkit/Utilities/png .h
  png pngconf pngDllConfig
)
