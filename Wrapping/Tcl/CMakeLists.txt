INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                    ${CABLE_INCLUDE_DIR}
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
                    ${ITK_SOURCE_DIR}/Wrapping/Tcl)

SUBDIRS(Numerics Common BasicFilters IO Algorithms Executable)

IF(LIBRARY_OUTPUT_PATH)
  SET(ITK_TCL_PACKAGE_DIR_BUILD "${LIBRARY_OUTPUT_PATH}")
ELSE(LIBRARY_OUTPUT_PATH)
  SET(ITK_TCL_PACKAGE_DIR_BUILD "${ITK_BINARY_DIR}/Wrapping/Tcl/$subdir")
ENDIF(LIBRARY_OUTPUT_PATH)

IF(UNIX)
  # Configure pkgIndex.tcl for the build tree.
  SET(ITK_TCL_PACKAGE_DIR "${ITK_TCL_PACKAGE_DIR_BUILD}")
  SET(ITK_TCL_DATA_ROOT "${ITK_DATA_ROOT}")
  SET(ITK_TCL_TEST_ROOT "${ITK_BINARY_DIR}/Testing")
  SET(ITK_TCL_SCRIPT_DIR "${ITK_SOURCE_DIR}/Wrapping/Tcl")
  SET(ITK_LIBNAME_PREFIX "lib")
  SET(ITK_MSDEV_CONFIG_DIR "")
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/pkgIndex.tcl IMMEDIATE)

  # Configure pkgIndex.tcl for installation.
  SET(ITK_TCL_DATA_ROOT "[file dirname [info script]]/Data")
  SET(ITK_TCL_TEST_ROOT "[file dirname [info script]]/Testing")
  SET(ITK_TCL_SCRIPT_DIR "[file dirname [info script]]")
  SET(ITK_TCL_PACKAGE_DIR "[file dirname [info script]]")
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/Install/Hide/pkgIndex.tcl IMMEDIATE)
ELSE(UNIX)
  # Configure pkgIndex.tcl for installation.  While installation doesn't
  # actually do anything on Windows, this generated file will be useful
  # to those creating binary distribution packages.
  SET(ITK_TCL_DATA_ROOT "[file dirname [info script]]/Data")
  SET(ITK_TCL_TEST_ROOT "[file dirname [info script]]/Testing")
  SET(ITK_TCL_SCRIPT_DIR "[file dirname [info script]]")
  SET(ITK_TCL_PACKAGE_DIR "[file dirname [info script]]")
  SET(ITK_MSDEV_CONFIG_DIR "")
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                 ${ITK_BINARY_DIR}/Wrapping/Tcl/Install/Hide/pkgIndex.tcl IMMEDIATE)

  # Configure pkgIndex.tcl for the build tree in each possible configuration.
  SET(ITK_TCL_PACKAGE_DIR "${ITK_TCL_PACKAGE_DIR_BUILD}")
  SET(ITK_TCL_DATA_ROOT "${ITK_DATA_ROOT}")
  SET(ITK_TCL_TEST_ROOT "${ITK_BINARY_DIR}/Testing")
  SET(ITK_TCL_SCRIPT_DIR "${ITK_SOURCE_DIR}/Wrapping/Tcl")
  SET(ITK_LIBNAME_PREFIX "")
  FOREACH (config Debug Release RelWithDebInfo MinSizeRel)
    SET(ITK_MSDEV_CONFIG_DIR ${config})
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/Tcl/pkgIndex.tcl.in
                   ${ITK_BINARY_DIR}/Wrapping/Tcl/${config}/pkgIndex.tcl IMMEDIATE)
  ENDFOREACH (config)
ENDIF(UNIX)

INSTALL_PROGRAMS(/bin "itktcl")
INSTALL_FILES(/lib/InsightToolkit ".tcl" itkdata itkinteraction itktesting itkutils)

# Temporary check for CMake 1.5 or 1.6.
IF (CMAKE_MAJOR_VERSION MATCHES "^1$")
  IF (CMAKE_MINOR_VERSION MATCHES "^[56]$")
    INSTALL_FILES(/lib/InsightToolkit FILES "${ITK_BINARY_DIR}/Wrapping/Tcl/Install/Hide/pkgIndex.tcl")
  ENDIF (CMAKE_MINOR_VERSION MATCHES "^[56]$")
ENDIF (CMAKE_MAJOR_VERSION MATCHES "^1$")
