INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)
INCLUDE(${CABLE_SOURCE_DIR}/CxxTypes/CMakeLists_Sources.txt)

INCLUDE_DIRECTORIES(
${TCL_INCLUDE_PATH}
${CABLE_SOURCE_DIR}/WrapTclFacility
)

LINK_DIRECTORIES(
${CABLE_BINARY_DIR}/WrapTclFacility
)

LINK_LIBRARIES(
${TCL_LIBRARY}
)

ADD_DEFINITIONS(-DCxxTypes_EXPORTS)

ADD_LIBRARY(WrapTclFacility SHARED CxxTypes_SRCS
wrapArgument
wrapCalls
wrapConstructor
wrapConversionTable
wrapCxxObject
wrapFunctionBase
wrapFunctionSelector
wrapInit
wrapMethod
wrapPointerToPointerMap
wrapStaticMethod
wrapTclCxxObject
wrapTypeInfo
wrapUtils
wrapWrapperBase
wrapWrapperFacility
)

IF (NOT CABLE_NO_INSTALL)
  INSTALL_FILES(/share/Cable/include/WrapTclFacility ^wrap.*\\.h$)
  INSTALL_FILES(/share/Cable/include/WrapTclFacility ^wrap.*\\.txx$)
  INSTALL_TARGETS(/share/Cable/lib WrapTclFacility)
  SET (CABLE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
  CONFIGURE_FILE(${CABLE_SOURCE_DIR}/WrapTclFacility/pkgIndex.tcl.in
                 ${CABLE_BINARY_DIR}/WrapTclFacility/pkgIndex.tcl)
  INSTALL_FILES(/share/Cable/lib "" pkgIndex.tcl)
ENDIF (NOT CABLE_NO_INSTALL)
