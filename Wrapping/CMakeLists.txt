INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c|ce|wrap_).*$")
INCLUDE(${CMAKE_ROOT}/Modules/FindGCCXML.cmake)

IF(GCCXML)
  SET(ITK_GCCXML_EXECUTABLE "${GCCXML}")
ENDIF(GCCXML)

# Find CABLE.
INCLUDE(${ITK_SOURCE_DIR}/Wrapping/FindCABLE.cmake)
IF(CABLE_USE_FILE)
  INCLUDE(${CABLE_USE_FILE})

  IF(CABLE_CXX_MISMATCH)
    MESSAGE(SEND_ERROR
            "The selected CABLE was built with "
            "\"${CABLE_CXX_COMPILER}\""
            ", but the current compiler is \"${CMAKE_CXX_COMPILER}\".")
    SET(ITK_WRAP_TCL 0)
  ENDIF(CABLE_CXX_MISMATCH)

  # Make sure Tcl is supported by the CABLE that was found.
  IF(NOT CABLE_TCL_LIBRARY)
    MESSAGE(SEND_ERROR "The selected CABLE does not support Tcl!")
    SET(ITK_WRAP_TCL 0)
  ENDIF(NOT CABLE_TCL_LIBRARY)

ELSE(CABLE_USE_FILE)
  MESSAGE(SEND_ERROR "Wrapping is enabled, but CABLE_USE_FILE is not found!")
  SET(ITK_WRAP_TCL 0)
ENDIF(CABLE_USE_FILE)

IF (CMAKE_MAJOR_VERSION MATCHES "^1$")
  IF (CMAKE_MINOR_VERSION MATCHES "^[56]$")
    SET(ITK_CMAKE_HAS_EXPLICIT_INSTALL_FILES 1)
  ENDIF (CMAKE_MINOR_VERSION MATCHES "^[56]$")
ENDIF (CMAKE_MAJOR_VERSION MATCHES "^1$")

#-----------------------------------------------------------------------------
# Build Tcl wrapper tree if ITK_WRAP_TCL is on.
IF(ITK_WRAP_TCL)
  SUBDIRS(Tcl)

  IF(NOT ITK_CMAKE_HAS_EXPLICIT_INSTALL_FILES)
    # Hack for installation with CMake 1.4.
    SET(ITK_TCL_PKGINDEX_FOR_INSTALL "${ITK_BINARY_DIR}/Wrapping/pkgIndex.tcl")
    INSTALL_FILES(/lib/InsightToolkit ".tcl" "pkgIndex")
  ENDIF(NOT ITK_CMAKE_HAS_EXPLICIT_INSTALL_FILES)
ENDIF(ITK_WRAP_TCL)
