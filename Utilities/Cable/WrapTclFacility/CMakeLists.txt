LIBRARY(WrapTclFacility)

SOURCE_FILES(
wrapInit
wrapInstanceTable
wrapPointer
wrapReference
wrapUtils
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN wrapDllAllocator)

LINK_LIBRARIES(
CxxTypes
)

UNIX_LIBRARIES(
-ltcl8.2
)
