LIBRARY(WrapTclFacility)

SOURCE_FILES(
wrapCalls
wrapConversionTable
wrapInit
wrapInstanceTable
wrapPointer
wrapReference
wrapTypeInfo
wrapUtils
wrapWrapperBase
wrapWrapperTable
)

LINK_LIBRARIES(
CxxTypes
)

WIN32_DEFINES(-D_wrap_DLL)
