INCLUDE(${CMAKE_ROOT}/Modules/FindGCCXML.cmake)

IF(NOT GCCXML)
  MESSAGE("Wrapping is enabled, but GCCXML is not found!" "Error")
ENDIF(NOT GCCXML)

IF(ITK_WRAP_TCL)
  SUBDIRS(Tcl)
ENDIF(ITK_WRAP_TCL)

