INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                    ${TK_INCLUDE_PATH}
                    ${CABLE_INCLUDE_DIR}
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
                    ${ITK_SOURCE_DIR}/Wrapping/Tcl)

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
  INSTALL_FILES(/lib/InsightToolkit FILES "${CABLE_TCL_LIBRARY}")
ENDIF(ITK_CMAKE_HAS_EXPLICIT_INSTALL_FILES)

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the build tree.

IF(CMAKE_BUILD_TOOL MATCHES "^(msdev|devenv)$")
  SET(ITK_MSDEV_CONFIG_DIRS "Debug;Release;RelWithDebInfo;MinSizeRel")
ENDIF(CMAKE_BUILD_TOOL MATCHES "^(msdev|devenv)$")

SET(ITK_TCL_DATA_ROOT "${ITK_DATA_ROOT}")
SET(ITK_TCL_TEST_ROOT "${ITK_BINARY_DIR}/Testing")
SET(ITK_TCL_SCRIPT_DIR "${ITK_SOURCE_DIR}/Wrapping/Tcl")

IF(ITK_MSDEV_CONFIG_DIRS)
  FOREACH (config ${ITK_MSDEV_CONFIG_DIRS})
    SET(ITK_TCL_PACKAGE_DIR "${ITK_TCL_PACKAGE_DIR_BUILD}/${config}")
    CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in"
                   "${ITK_BINARY_DIR}/Wrapping/Tcl/${config}/pkgIndex.tcl" IMMEDIATE @ONLY)
  ENDFOREACH (config)
ELSE(ITK_MSDEV_CONFIG_DIRS)
  SET(ITK_TCL_PACKAGE_DIR "${ITK_TCL_PACKAGE_DIR_BUILD}")
  CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in"
                 "${ITK_BINARY_DIR}/Wrapping/Tcl/pkgIndex.tcl" IMMEDIATE @ONLY)
ENDIF(ITK_MSDEV_CONFIG_DIRS)

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the install tree.

SET(ITK_TCL_SCRIPT_DIR "[file dirname [info script]]")
SET(ITK_TCL_PACKAGE_DIR "[file dirname [info script]]")
SET(ITK_TCL_DATA_ROOT "[file dirname [info script]]/Data")
SET(ITK_TCL_TEST_ROOT "<NO_DEFAULT>")
CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in"
               "${ITK_TCL_PKGINDEX_FOR_INSTALL}" IMMEDIATE @ONLY)
