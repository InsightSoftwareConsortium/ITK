PROJECT(EXPAT)

INCLUDE_DIRECTORIES(
${EXPAT_SOURCE_DIR}/xmlparse
${EXPAT_SOURCE_DIR}/xmltok
${EXPAT_SOURCE_DIR}/xmlwf
)

SOURCE_FILES(Expat_SRCS
xmlparse/xmlparse
xmlparse/hashtable
xmltok/xmltok
xmltok/xmlrole
xmlwf/xmlwf
xmlwf/xmlfile
xmlwf/codepage
) 

IF(UNIX)
  SOURCE_FILES(Expat_SRCS xmlwf/unixfilemap)
ELSE(UNIX)
  SOURCE_FILES(Expat_SRCS xmlwf/win32filemap)
ENDIF(UNIX)

OPTION(BUILD_EXPAT_SHARED "Build the expat library as a shared library." OFF)

IF(BUILD_EXPAT_SHARED)
  SET(EXPAT_LIBRARY_TYPE SHARED)
ELSE(BUILD_EXPAT_SHARED)
  SET(EXPAT_LIBRARY_TYPE STATIC)
ENDIF(BUILD_EXPAT_SHARED)

ADD_LIBRARY(Expat ${EXPAT_LIBRARY_TYPE} Expat_SRCS)

IF(WIN32)
  IF(BUILD_EXPAT_SHARED)
    ADD_DEFINITIONS(-DXMLPARSEAPI=__declspec(dllexport))
  ENDIF(BUILD_EXPAT_SHARED)
ENDIF(WIN32)

