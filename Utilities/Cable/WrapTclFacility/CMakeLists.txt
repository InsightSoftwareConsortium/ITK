LIBRARY(WrapTclFacility)

SOURCE_FILES(
wrapInstances
wrapPointer
wrapReference
wrapUtils
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN wrapDllAllocator)

LINK_LIBRARIES(
CxxTypes
)

