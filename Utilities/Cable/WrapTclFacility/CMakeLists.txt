LIBRARY(WrapTclFacility)

SOURCE_FILES(
wrapPointer
wrapReference
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN wrapDllAllocator)
