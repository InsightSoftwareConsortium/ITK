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

ADD_LIBRARY(Expat STATIC Expat_SRCS)
