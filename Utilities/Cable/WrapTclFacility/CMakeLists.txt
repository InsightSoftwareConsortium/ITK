LIBRARY(WrapTclFacility)

SOURCE_FILES(
wrapCalls
wrapInit
wrapInstanceTable
wrapPointer
wrapReference
wrapTypeSystemTable
wrapUtils
wrapWrapperBase
wrapWrapperTable
)

LINK_LIBRARIES(
CxxTypes
)

WIN32_DEFINES(-D_wrap_DLL)
