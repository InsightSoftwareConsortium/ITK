INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                    ${TK_INCLUDE_PATH}
                    ${CABLE_INCLUDE_DIRS}
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
                    ${ITK_SOURCE_DIR}/Wrapping/Tcl)

LINK_DIRECTORIES(${CABLE_LIBRARY_DIRS})

SUBDIRS(Numerics Common BasicFilters IO Algorithms Executable)

IF(UNIX)
  SET(ITK_TCL_LIBNAME_PREFIX "lib")
ELSE(UNIX)
  SET(ITK_TCL_LIBNAME_PREFIX "")
ENDIF(UNIX)

SET(ITK_TCL_PACKAGE_DIR_BUILD "${ITK_LIBRARY_PATH}")

#-----------------------------------------------------------------------------
# Setup install targets.
INSTALL_PROGRAMS(/bin "itktcl")
INSTALL_FILES(/lib/InsightToolkit ".tcl" itkdata itkinteraction itktesting itkutils)

IF(ITK_CMAKE_HAS_EXPLICIT_INSTALL_FILES)
  SET(ITK_TCL_PKGINDEX_FOR_INSTALL "${ITK_BINARY_DIR}/Wrapping/Tcl/Install/Hide/pkgIndex.tcl")
  INSTALL_FILES(/lib/InsightToolkit FILES "${ITK_TCL_PKGINDEX_FOR_INSTALL}")
ENDIF(ITK_CMAKE_HAS_EXPLICIT_INSTALL_FILES)

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the build tree.

SET(ITK_TCL_CONFIGURATION_TYPES ${CMAKE_CONFIGURATION_TYPES})
SET(ITK_TCL_DATA_ROOT "${ITK_DATA_ROOT}")
SET(ITK_TCL_TEST_ROOT "${ITK_BINARY_DIR}/Testing")
SET(ITK_TCL_SCRIPT_DIR "${ITK_SOURCE_DIR}/Wrapping/Tcl")

IF(ITK_TCL_CONFIGURATION_TYPES)
  FOREACH(config ${ITK_TCL_CONFIGURATION_TYPES})
    SET(ITK_TCL_PACKAGE_DIR "${ITK_TCL_PACKAGE_DIR_BUILD}/${config}")
    CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in"
                   "${ITK_BINARY_DIR}/Wrapping/Tcl/${config}/pkgIndex.tcl"
                   @ONLY IMMEDIATE)
  ENDFOREACH(config)
ELSE(ITK_TCL_CONFIGURATION_TYPES)
  SET(ITK_TCL_PACKAGE_DIR "${ITK_TCL_PACKAGE_DIR_BUILD}")
  CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in"
                 "${ITK_BINARY_DIR}/Wrapping/Tcl/pkgIndex.tcl"
                 @ONLY IMMEDIATE)
ENDIF(ITK_TCL_CONFIGURATION_TYPES)

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the install tree.

SET(ITK_TCL_SCRIPT_DIR "[file dirname [info script]]")
SET(ITK_TCL_PACKAGE_DIR "[file dirname [info script]]")
SET(ITK_TCL_DATA_ROOT "[file dirname [info script]]/Data")
SET(ITK_TCL_TEST_ROOT "<NO_DEFAULT>")
CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in"
               "${ITK_TCL_PKGINDEX_FOR_INSTALL}" IMMEDIATE @ONLY)
