SUBDIRS(Static)

# If building the Tcl wrapping facility, we need a shared CxxTypes library.
IF(CABLE_TCL_FACILITY)
  SUBDIRS(Shared)
ENDIF(CABLE_TCL_FACILITY)
