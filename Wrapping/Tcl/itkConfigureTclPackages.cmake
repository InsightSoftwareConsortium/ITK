IF(LIBRARY_OUTPUT_PATH)
  SET(ITK_TCL_PACKAGE_DIR ${LIBRARY_OUTPUT_PATH})
ELSE(LIBRARY_OUTPUT_PATH)
  SET(ITK_TCL_PACKAGE_DIR ${ITK_BINARY_DIR}/Wrapping/Tcl/$subdir)
ENDIF(LIBRARY_OUTPUT_PATH)

IF(UNIX)
  SET(ITK_LIBNAME_PREFIX "lib")
  SET(ITK_MSDEV_CONFIG_DIR "")
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/pkgIndex.tcl IMMEDIATE)
ELSE(UNIX)
  SET(ITK_LIBNAME_PREFIX "")
  FOREACH (config Debug Release RelWithDebInfo MinSizeRel)
    SET(ITK_MSDEV_CONFIG_DIR ${config})
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                   ${ITK_BINARY_DIR}/Wrapping/Tcl/${config}/pkgIndex.tcl IMMEDIATE)
  ENDFOREACH (config)
ENDIF(UNIX)
