PROJECT(ITKEXPAT)
INCLUDE_REGULAR_EXPRESSION("^(expat|xml|ascii|utf|name|iascii|latin).*$")

SET(expat_SRCS
xmlparse.c  xmlrole.c  xmltok.c
)

INCLUDE_DIRECTORIES(${ITKEXPAT_SOURCE_DIR})
INCLUDE_DIRECTORIES(${ITKEXPAT_BINARY_DIR})

IF(WIN32)
  IF(NOT BUILD_SHARED_LIBS)
    SET (ITK_EXPAT_STATIC 1)
  ENDIF(NOT BUILD_SHARED_LIBS)
ENDIF(WIN32)

CONFIGURE_FILE(${ITKEXPAT_SOURCE_DIR}/expatConfig.h.in
               ${ITKEXPAT_BINARY_DIR}/expatConfig.h)
CONFIGURE_FILE(${ITKEXPAT_SOURCE_DIR}/expatDllConfig.h.in
               ${ITKEXPAT_BINARY_DIR}/expatDllConfig.h)

ADD_LIBRARY(ITKEXPAT ${expat_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit ITKEXPAT)
INSTALL_FILES(/include/InsightToolkit/expat .h expat expatDllConfig)
