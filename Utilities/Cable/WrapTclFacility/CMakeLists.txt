SOURCE_FILES(WrapTclFacility_SRCS
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

ADD_LIBRARY(WrapTclFacility WrapTclFacility_SRCS)

LINK_LIBRARIES(
CxxTypes
)

WIN32_DEFINES(-D_wrap_LIBRARY)
