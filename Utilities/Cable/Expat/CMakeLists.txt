LIBRARY(Expat)

AUX_SOURCE_DIRECTORY(xmlparse/dummy)
AUX_SOURCE_DIRECTORY(xmltok/dummy)
AUX_SOURCE_DIRECTORY(xmlwf/dummy)

SOURCE_FILES(
xmlparse/xmlparse
xmlparse/hashtable
xmltok/xmltok
xmltok/xmlrole
xmlwf/xmlwf
xmlwf/xmlfile
xmlwf/codepage
)

SOURCE_FILES_REQUIRE(UNIX SOURCES_BEGIN
xmlwf/unixfilemap
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN
xmlwf/win32filemap
)

INCLUDE_DIRECTORIES(
${CABLE_SOURCE_DIR}/Expat/xmlparse
${CABLE_SOURCE_DIR}/Expat/xmltok
${CABLE_SOURCE_DIR}/Expat/xmlwf
)

