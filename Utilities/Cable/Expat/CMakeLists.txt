AUX_SOURCE_DIRECTORY(xmlparse/dummy Expat_SRCS)
AUX_SOURCE_DIRECTORY(xmltok/dummy Expat_SRCS)
AUX_SOURCE_DIRECTORY(xmlwf/dummy Expat_SRCS)

SOURCE_FILES(Expat_SRCS
xmlparse/xmlparse
xmlparse/hashtable
xmltok/xmltok
xmltok/xmlrole
xmlwf/xmlwf
xmlwf/xmlfile
xmlwf/codepage
xmlwf/filemap
) 

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Expat/xmlparse
${CABLE_SOURCE_DIR}/Expat/xmltok
${CABLE_SOURCE_DIR}/Expat/xmlwf
)

ADD_LIBRARY(Expat Expat_SRCS)

