INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

SET(BUILD_SHARED_LIBS ON)

INCLUDE_DIRECTORIES(
${TCL_INCLUDE_PATH}
${CABLE_SOURCE_DIR}/WrapTclFacility
)

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

TARGET_LINK_LIBRARIES(wrapTclShell WrapTclFacility)

IF (WIN32)
  ADD_DEFINITIONS(-D_wrap_SHARED -D_cxx_SHARED)
ENDIF (WIN32)
