PROJECT (CABLE)
INCLUDE_REGULAR_EXPRESSION ("^(cxx|cable|xml|gen|wrap).*$")

SUBDIRS (CxxTypes Expat Parsers Generators Executables)

OPTION (CABLE_TCL_FACILITY "Build CABLE's Tcl wrapping facility." OFF)
IF (CABLE_TCL_FACILITY)
  SUBDIRS(WrapTclFacility)
ENDIF (CABLE_TCL_FACILITY)

INCLUDE_DIRECTORIES (
${CABLE_SOURCE_DIR}/CxxTypes
${CABLE_SOURCE_DIR}/Expat/xmlparse
)
