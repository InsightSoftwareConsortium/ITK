INCLUDE(${CMAKE_ROOT}/Modules/FindCABLE.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/FindGCCXML.cmake)

IF(NOT GCCXML)
  MESSAGE("Wrapping is enabled, but GCCXML is not found!" "Error")
ENDIF(NOT GCCXML)

IF(NOT CABLE)
  MESSAGE("Wrapping is enabled, but CABLE is not found!" "Error")
ENDIF(NOT CABLE)

IF(ITK_WRAP_TCL)
  SUBDIRS(Tcl)
ENDIF(ITK_WRAP_TCL)
