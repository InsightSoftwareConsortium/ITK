SOURCE_FILES(WrapTclFacility_SRCS
wrapCalls
wrapConversionTable
wrapFunctionBase
wrapFunctionSelector
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

IF(WIN32)
  ADD_DEFINITIONS(-D_wrap_LIBRARY)
ENDIF(WIN32)
