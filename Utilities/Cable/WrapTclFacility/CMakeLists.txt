INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

INCLUDE_DIRECTORIES(${CABLE_SOURCE_DIR}/WrapTclFacility)
LINK_DIRECTORIES(${CABLE_BINARY_DIR}/WrapTclFacility)

LINK_LIBRARIES(
${TCL_LIBRARY}
CxxTypes
)

ADD_LIBRARY(WrapTclFacility
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

ADD_EXECUTABLE(wrapTclShell wrapTclShell)

TARGET_LINK_LIBRARIES(wrapTclShell
WrapTclFacility
)
