INCLUDE(${CMAKE_ROOT}/Modules/FindCABLE.cmake)

IF(NOT CABLE)
  MESSAGE("Wrapping is enabled, but CABLE is not found!" "Error")
ENDIF(NOT CABLE)

IF(ITK_WRAP_TCL)
  SUBDIRS(Tcl)
ENDIF(ITK_WRAP_TCL)
