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

SOURCE_FILES(WrapTclFacility_SRCS
wrapArgument
wrapCalls
wrapConstructor
wrapCxxObject
wrapFunctionBase
wrapFunctionSelector
wrapInit
wrapMethod
wrapStaticMethod
wrapTclCxxObject
wrapTypeInfo
wrapUtils
wrapWrapperBase
wrapWrapperFacility
)

IF (WIN32)
  SOURCE_FILES(WrapTclFacility_SRCS wrapWin32OutputWindow)
ENDIF (WIN32)

ADD_LIBRARY(WrapTclFacility SHARED CxxTypes_SRCS WrapTclFacility_SRCS)  

IF (NOT CABLE_NO_INSTALL)
  INSTALL_FILES(/share/Cable/include/WrapTclFacility ^wrap.*\\.h$)
  INSTALL_FILES(/share/Cable/include/WrapTclFacility ^wrap.*\\.txx$)
  INSTALL_TARGETS(/share/Cable/lib WrapTclFacility)
  SET (CABLE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
  CONFIGURE_FILE(${CABLE_SOURCE_DIR}/WrapTclFacility/pkgIndex.tcl.in
                 ${CABLE_BINARY_DIR}/WrapTclFacility/pkgIndex.tcl)
  INSTALL_FILES(/share/Cable/lib "" pkgIndex.tcl)
ENDIF (NOT CABLE_NO_INSTALL)
