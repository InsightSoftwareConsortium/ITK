LIBRARY(WrapTclFacility)

SOURCE_FILES(
wrapInit
wrapInstanceTable
wrapPointer
wrapReference
wrapUtils
wrapWrapperTable
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN wrapDllAllocator)
