# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 1.4)
PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c|ce).*$")

SET(ITK_VERSION_MAJOR "1")
SET(ITK_VERSION_MINOR "1")
SET(ITK_VERSION_PATCH "0")

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
SET(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

IF(NOT WIN32)
  OPTION(BUILD_SHARED_LIBS "Build with shared libraries." OFF)
ENDIF(NOT WIN32)

SOURCE_GROUP("XML Files" "[.]xml$")

# Include the include and link libraries
INCLUDE (${ITK_SOURCE_DIR}/itkCMakeOptions.cmake)

SET (LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
SET (EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
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

# Build the Auxiliary subdirectory. It has its own 
# checking for dependencies on third party libraries
SUBDIRS(Auxiliary)


# Add an option to build or not build the Applications
OPTION(BUILD_APPLICATIONS "Build the applications directory")
IF(BUILD_APPLICATIONS)
  SUBDIRS(Applications)
ENDIF(BUILD_APPLICATIONS)

# Add an option to build or not build the Examples
OPTION(BUILD_EXAMPLES "Build the Examples directory" 1)
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
OPTION(USE_VTK "Use VTK (The Visualization Toolkit) (some applications need this)" 0)
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
    SET(HAS_VTK 0 CACHE INTERNAL "Use VTK: The Visualization Toolkit, this is required for some applications but not for the ITK toolkit")
  ENDIF (USE_VTK_FILE)
ELSE(USE_VTK)
  SET(HAS_VTK 0)
ENDIF(USE_VTK)


# Look for OpenGL
INCLUDE (${ITK_SOURCE_DIR}/Utilities/ITKFindOpenGL.cmake)
  
# Add an option to use or not use GLUT
OPTION(USE_GLUT "Use GLUT, this enables a very simple image viewer" 0)
IF(USE_GLUT)
  # Look for GLUT and if found build the simple image viewer
  INCLUDE (${ITK_SOURCE_DIR}/Utilities/ITKFindGLUT.cmake)
ENDIF(USE_GLUT)


# Add an option to use or not use FLTK (http://www.fltk.org)
OPTION(USE_FLTK "Use FLTK (The Fast Light Toolkit) for GUI (some applications need this)" 0)
IF(USE_FLTK)
  INCLUDE (${CMAKE_ROOT}/Modules/FindFLTK.cmake)
  IF(FLTK_VERSION_1.0.11)
    MESSAGE(SEND_ERROR "FLTK Version 1.0.11 is no longer supported in ITK. Please upgrade to FLTK version 1.1")
  ENDIF(FLTK_VERSION_1.0.11)
ENDIF(USE_FLTK)

# Copy the UseITK.cmake file to the binary tree for backward compatability.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake.in
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY IMMEDIATE)

INSTALL_FILES(/include/InsightToolkit ".h" "itkConfigure")
INSTALL_FILES(/lib/InsightToolkit ".cmake" "UseITK")

#-----------------------------------------------------------------------------
# Some common settings for ITKConfig.cmake file configuration.

# The preprocessor definitions.
IF(CMAKE_USE_PTHREADS)
  SET(ITK_DEFINITIONS ${ITK_DEFINITIONS} "-D_PTHREADS")
ENDIF(CMAKE_USE_PTHREADS)
IF(WIN32)
  SET(ITK_DEFINITIONS ${ITK_DEFINITIONS} "-DNOMINMAX")
ENDIF(WIN32)
IF(NOT ITK_USE_SYSTEM_STDLIB)
  IF(CMAKE_COMPILER_IS_GNUCXX)
    SET(ITK_DEFINITIONS ${ITK_DEFINITIONS} "-D__STL_USE_NEW_IOSTREAMS")
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the build tree.

# The include directories.
SET(ITK_INCLUDE_DIRS
  "${ITK_BINARY_DIR}"
  "${ITK_BINARY_DIR}/Code/Numerics/vxl"
  "${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl"
  "${ITK_SOURCE_DIR}/Code/Algorithms"
  "${ITK_SOURCE_DIR}/Code/BasicFilters"
  "${ITK_SOURCE_DIR}/Code/Common"
  "${ITK_SOURCE_DIR}/Code/IO"
  "${ITK_SOURCE_DIR}/Code/Numerics/FEM"
  "${ITK_SOURCE_DIR}/Code/Numerics"
  "${ITK_SOURCE_DIR}/Code/Numerics/Statistics"
  "${ITK_SOURCE_DIR}/Code/Numerics/vxl"
  "${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl"
  "${ITK_SOURCE_DIR}/Code/SpatialObject"
  "${ITK_SOURCE_DIR}/Utilities/GlutMaster"
  "${ITK_SOURCE_DIR}/Utilities/MetaIO"
  "${ITK_SOURCE_DIR}/Utilities/png"
  "${ITK_SOURCE_DIR}/Utilities/zlib"
  "${ITK_SOURCE_DIR}/Auxiliary/FltkImageViewer"
  "${ITK_BINARY_DIR}/Auxiliary/FltkImageViewer"
  "${ITK_SOURCE_DIR}/Auxiliary/VtkFltk"
  "${ITK_BINARY_DIR}/Auxiliary/VtkFltk"
)
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS ${ITK_INCLUDE_DIRS} "${ITK_SOURCE_DIR}/Utilities/stdlib")
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

SET(ITK_LIBRARY_DIRS "${ITK_LIBRARY_PATH}")
SET(ITK_USE_FILE "${ITK_BINARY_DIR}/UseITK.cmake")
CONFIGURE_FILE("${ITK_SOURCE_DIR}/ITKConfig.cmake.in"
               "${ITK_BINARY_DIR}/ITKConfig.cmake" IMMEDIATE @ONLY)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the install tree.

SET(ITK_INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include/InsightToolkit")
SET(ITK_INCLUDE_DIRS
  "${ITK_INSTALL_INCLUDE_DIR}"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl/vcl"
  "${ITK_INSTALL_INCLUDE_DIR}/Algorithms"
  "${ITK_INSTALL_INCLUDE_DIR}/BasicFilters"
  "${ITK_INSTALL_INCLUDE_DIR}/Common"
  "${ITK_INSTALL_INCLUDE_DIR}/IO"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/FEM"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/Statistics"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl/vcl"
  "${ITK_INSTALL_INCLUDE_DIR}/SpatialObject"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/GlutMaster"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/png"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/zlib"
  "${ITK_INSTALL_INCLUDE_DIR}/Auxiliary/FltkImageViewer"
  "${ITK_INSTALL_INCLUDE_DIR}/Auxiliary/VtkFltk"
)
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS ${ITK_INCLUDE_DIRS} "${ITK_INSTALL_INCLUDE_DIR}/Utilities/stdlib")
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

SET(ITK_LIBRARY_DIRS "${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit")
SET(ITK_USE_FILE "${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/UseITK.cmake")
CONFIGURE_FILE("${ITK_SOURCE_DIR}/ITKConfig.cmake.in"
               "${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake" IMMEDIATE @ONLY)
