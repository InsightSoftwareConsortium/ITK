# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 1.4)
PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c|ce).*$")

IF(NOT WIN32)
  OPTION(BUILD_SHARED_LIBS "Build with shared libraries." OFF)
ENDIF(NOT WIN32)

SOURCE_GROUP("XML Files" "[.]xml$")

# Include the include and link libraries
INCLUDE (${ITK_SOURCE_DIR}/itkCMakeOptions.cmake)

SET (LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin/ CACHE INTERNAL "Single output directory for building all libraries.")
SET (EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin/ CACHE INTERNAL "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

SET (ITK_LIBRARY_PATH ${LIBRARY_OUTPUT_PATH} CACHE INTERNAL "Where ITK libraries are. Used by projects which depend on ITK. Do not remove.")

SET (ITK_EXECUTABLE_PATH ${EXECUTABLE_OUTPUT_PATH} CACHE INTERNAL "Where ITK executables are. Used by projects which depend on ITK. Do not remove.")

# where will executable tests be written ?
SET (CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})


# Walk into these directories before Testing or Auxiliary
SUBDIRS(Utilities Code)

# Optionally add Tcl wrappers.  They should be built before Testing so
# that that Tcl tests can run when Tcl is available.
OPTION(ITK_WRAP_TCL "Build Tcl wrapper support." OFF)
IF(ITK_WRAP_TCL)
  SUBDIRS(Wrapping)
ENDIF(ITK_WRAP_TCL)

# Include the standard Dart testing module
INCLUDE (${CMAKE_ROOT}/Modules/Dart.cmake)
IF (BUILD_TESTING)
  SUBDIRS(Testing)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/InsightLogo.gif ${ITK_BINARY_DIR}/Testing/HTML/TestingResults/Icons/Logo.gif COPYONLY)
FIND_PATH (ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})
ENDIF (BUILD_TESTING)

CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY)

# Build the Auxiliary subdirectory. It has its own 
# checking for dependencies on third party libraries
SUBDIRS(Auxiliary)


# Add an option to build or not build the Examples
OPTION(BUILD_EXAMPLES "Build the examples directory")
IF(BUILD_EXAMPLES)
  SUBDIRS(Examples)
ENDIF(BUILD_EXAMPLES)

# Configure files
CONFIGURE_FILE(
${ITK_SOURCE_DIR}/itkConfigure.h.in 
${ITK_BINARY_DIR}/itkConfigure.h)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Utilities/Doxygen/doxygen.config.in
${ITK_BINARY_DIR}/doxygen.config)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
${ITK_BINARY_DIR}/itkdoxygen.pl)

# make directories in the binary tree
MAKE_DIRECTORY(${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl)
MAKE_DIRECTORY(${ITK_BINARY_DIR}/Documentation/Doxygen)

# run VXL configure scripts
IF(RUN_CONFIGURE)
  EXEC_PROGRAM(${ITK_SOURCE_DIR}/Code/Numerics/vxl/configure 
               ${ITK_BINARY_DIR}/Code/Numerics/vxl)
  EXEC_PROGRAM(${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/configure 
               ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl)
ENDIF(RUN_CONFIGURE)

# for win32 vc60, just copy files
IF(NOT UNIX)
  IF(WIN32)
    CONFIGURE_FILE(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vxl_config.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vxl_config.h COPYONLY)
    CONFIGURE_FILE(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_compiler.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_compiler.h COPYONLY)
    CONFIGURE_FILE(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_headers.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_headers.h COPYONLY)
    CONFIGURE_FILE(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_manual.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_manual.h COPYONLY)
  ENDIF(WIN32)
ENDIF(NOT UNIX)

# Add an option to use or not use VTK
OPTION(USE_VTK "Use VTK (The Visualization Toolkit) (some examples need this)" 0)
IF(USE_VTK)
  INCLUDE (${ITK_SOURCE_DIR}/Utilities/ITKFindVTK.cmake)
  IF (USE_VTK_FILE)
    INCLUDE (${USE_VTK_FILE})
    # Test for VTK using std libs
    IF(VTK_USE_ANSI_STDLIB)
      SET(HAS_VTK 1)
    ELSE( VTK_USE_ANSI_STDLIB )
      MESSAGE ("Warning. Your VTK was not built with the VTK_USE_ANSI_STDLIB option ON. \nPlease reconfigure and recompile VTK with this option before trying to use it with ITK.")
    ENDIF( VTK_USE_ANSI_STDLIB )
    # Test for VTK building Hybrid
    IF(VTK_USE_HYBRID)
      SET(HAS_VTK_HYBRID 1 CACHE INTERNAL "VTK Hybrid libraries are available")
    ELSE( VTK_USE_HYBRID )
      MESSAGE ("Warning. Your VTK was not built with the VTK_USE_HYBRID option ON. \nPlease reconfigure and recompile VTK with this option before trying to use it with ITK.")
    ENDIF( VTK_USE_HYBRID )
  ELSE (USE_VTK_FILE)
    MESSAGE("The UseVTK.cmake file was not found. You will not be able to build code requiring VTK")
    SET(HAS_VTK 0 CACHE INTERNAL "Use VTK: The Visualization Toolkit, this is required for some examples but not for the ITK toolkit")
  ENDIF (USE_VTK_FILE)
ELSE(USE_VTK)
  SET(HAS_VTK 0)
ENDIF(USE_VTK)


# Add an option to use or not use FLTK (http://www.fltk.org)
OPTION(USE_FLTK "Use FLTK (The Fast Light Toolkit) for GUI (some examples need this)" 0)
IF(USE_FLTK)
  INCLUDE (${CMAKE_ROOT}/Modules/FindFLTK.cmake)
  IF(FLTK_VERSION_1.0.11)
    MESSAGE(SEND_ERROR "FLTK Version 1.0.11 is no longer supported in ITK. Please upgrade to FLTK version 1.1")
  ENDIF(FLTK_VERSION_1.0.11)
ENDIF(USE_FLTK)


