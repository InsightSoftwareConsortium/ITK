IF(LIBRARY_OUTPUT_PATH)
  SET(ITK_WRAP_TCL_DIR ${LIBRARY_OUTPUT_PATH})
  SET(ITK_WRAP_TCL_SUBDIRS "")
ELSE(LIBRARY_OUTPUT_PATH)
  SET(ITK_WRAP_TCL_DIR ${ITK_BINARY_DIR}/Wrapping/Tcl)
  SET(ITK_WRAP_TCL_SUBDIRS "Common")
ENDIF(LIBRARY_OUTPUT_PATH)

SET(ITK_TCL_UTILS_DIR ${ITK_SOURCE_DIR}/Wrapping/Tcl)

IF(UNIX)
  SET(ITK_MSDEV_CONFIG_DIR "")
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/pkgIndex.tcl IMMEDIATE)
ELSE(UNIX)
  SET(ITK_MSDEV_CONFIG_DIR Debug)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/Debug/pkgIndex.tcl IMMEDIATE)
  SET(ITK_MSDEV_CONFIG_DIR Release)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/Release/pkgIndex.tcl IMMEDIATE)
  SET(ITK_MSDEV_CONFIG_DIR RelWithDebInfo)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/RelWithDebInfo/pkgIndex.tcl IMMEDIATE)
  SET(ITK_MSDEV_CONFIG_DIR MinSizeRel)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/MinSizeRel/pkgIndex.tcl IMMEDIATE)
ENDIF(UNIX)