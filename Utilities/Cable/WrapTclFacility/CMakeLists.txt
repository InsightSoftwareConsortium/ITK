SOURCE_FILES(WrapTclFacility_SRCS
wrapArgument
wrapCalls
wrapConstructor
wrapConversionTable
wrapFunctionBase
wrapFunctionSelector
wrapInit
wrapInstanceTable
wrapMethod
wrapPointer
wrapReference
wrapStaticMethod
wrapTypeInfo
wrapUtils
wrapWrapperBase
wrapWrapperFacility
)

ADD_LIBRARY(WrapTclFacility WrapTclFacility_SRCS)

LINK_LIBRARIES(
CxxTypes
)

IF(WIN32)
  ADD_DEFINITIONS(-D_wrap_LIBRARY)
ENDIF(WIN32)
