PROJECT(ITKPNG)
INCLUDE_REGULAR_EXPRESSION("^(png|itk|z).*$")

INCLUDE_DIRECTORIES(${ITKPNG_SOURCE_DIR})

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
TARGET_LINK_LIBRARIES(itkpng ${ITK_ZLIB_LIBRARIES})

# Apply user-defined properties to the library target.
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(itkpng PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL_TARGETS(
    ${ITK_INSTALL_LIB_DIR}
    RUNTIME_DIRECTORY ${ITK_INSTALL_BIN_DIR}
    itkpng)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_INCLUDE_DIR}/Utilities/itkpng .h
                png pngconf pngDllConfig itk_png_mangle)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
