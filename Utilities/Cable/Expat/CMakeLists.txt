MAKE_DIRECTORY(${CABLE_BINARY_DIR}/Expat/xmlparse)
MAKE_DIRECTORY(${CABLE_BINARY_DIR}/Expat/xmltok)
MAKE_DIRECTORY(${CABLE_BINARY_DIR}/Expat/xmlwf)

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Expat/xmlparse
${CABLE_SOURCE_DIR}/Expat/xmltok
${CABLE_SOURCE_DIR}/Expat/xmlwf
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

ADD_LIBRARY(Expat Expat_SRCS)

