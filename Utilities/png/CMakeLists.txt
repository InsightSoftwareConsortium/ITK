PROJECT(ITKPNG)

# source files for png
SET(PNG_SRCS
pngget.c    pngrio.c    pngwrite.c
png.c       pngmem.c    pngrtran.c  pngtrans.c  pngwtran.c
pngerror.c  pngpread.c  pngrutil.c  pngvcrd.c   pngwutil.c
pnggccrd.c  pngread.c   pngset.c   pngwio.c
)

CONFIGURE_FILE(
${ITKPNG_SOURCE_DIR}/.NoDartCoverage 
${ITKPNG_BINARY_DIR}/.NoDartCoverage)

IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DPNG_BUILD_DLL -DZLIB_DLL -DPNG_NO_MODULEDEF)
  ELSE(BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DPNG_STATIC)
  ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

ADD_LIBRARY(itkpng ${PNG_SRCS})
TARGET_LINK_LIBRARIES(itkpng itkzlib)

INSTALL_TARGETS(/lib/InsightToolkit itkpng)
INSTALL_FILES(/include/InsightToolkit/Utilities/png "(\\.h)$")
