INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                    ${CABLE_SOURCE_DIR}/CxxTypes
                    ${CABLE_SOURCE_DIR}/WrapTclFacility)

IF(NOT LIBRARY_OUTPUT_PATH)
  LINK_DIRECTORIES(${CABLE_BINARY_DIR}/CxxTypes
                   ${CABLE_BINARY_DIR}/WrapTclFacility)
ENDIF(NOT LIBRARY_OUTPUT_PATH)

SUBDIRS(Common)

INCLUDE(${ITK_SOURCE_DIR}/Wrapping/Tcl/itkConfigureTclPackages.cmake)
