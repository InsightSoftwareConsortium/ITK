PROJECT (CABLE)
INCLUDE_REGULAR_EXPRESSION ("^(cxx|cable|xml|gen|wrap).*$")

SUBDIRS (CxxTypes Expat Parsers Generators Executables)

OPTION (BUILD_TCL_FACILITY "Build the Tcl wrapping facility." OFF)
IF (BUILD_TCL_FACILITY)
  SUBDIRS(WrapTclFacility)
ENDIF (BUILD_TCL_FACILITY)

INCLUDE_DIRECTORIES (
${CABLE_SOURCE_DIR}/CxxTypes
${CABLE_SOURCE_DIR}/Expat/xmlparse
)

LINK_DIRECTORIES (
${CABLE_BINARY_DIR}/CxxTypes
${CABLE_BINARY_DIR}/Expat
${CABLE_BINARY_DIR}/Parsers
${CABLE_BINARY_DIR}/Generators
)

