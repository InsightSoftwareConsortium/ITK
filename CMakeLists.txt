# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 1.4)
PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c|ce).*$")
SOURCE_GROUP("XML Files" "[.]xml$")

#-----------------------------------------------------------------------------
# ITK version number.  An even minor number corresponds to releases.
SET(ITK_VERSION_MAJOR "1")
SET(ITK_VERSION_MINOR "1")
SET(ITK_VERSION_PATCH "0")

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
SET(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

#-----------------------------------------------------------------------------
# Test for some required system information.
IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
  INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityC.cmake)
  INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityCXX.cmake)
ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)

#-----------------------------------------------------------------------------
# Output directories.
SET (LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
SET (EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
SET(ITK_LIBRARY_PATH "${LIBRARY_OUTPUT_PATH}")
SET(ITK_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")

#-----------------------------------------------------------------------------
# ITK build configuration options.
IF(NOT WIN32)
  OPTION(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
ENDIF(NOT WIN32)

#-----------------------------------------------------------------------------
# VTK requires special compiler flags on some platforms.
IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -ftemplate-depth-50")
  IF(APPLE)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-long-double")
  ENDIF(APPLE)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

# Force the use of ansi cxx flags (i.e. -LANG:std on sgi )
IF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${CMAKE_ANSI_CXXFLAGS}")
ENDIF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
IF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} ${CMAKE_ANSI_CFLAGS}")
ENDIF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")

IF("x${CMAKE_TEMPLATE_FLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${CMAKE_TEMPLATE_FLAGS}")
ENDIF("x${CMAKE_TEMPLATE_FLAGS}" MATCHES "^x.*[^ ]")

IF(CMAKE_USE_PTHREADS)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D_PTHREADS")
ENDIF(CMAKE_USE_PTHREADS)
IF(WIN32)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -DNOMINMAX")
ENDIF(WIN32)

IF(NOT ITK_USE_SYSTEM_STDLIB)
  IF(CMAKE_COMPILER_IS_GNUCXX)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D__STL_USE_NEW_IOSTREAMS")
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Include the include and link libraries
INCLUDE (${ITK_SOURCE_DIR}/itkCMakeOptions.cmake)

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
 IF(NOT VXL_FROM_UTILITIES)
  IF(UNIX)
    EXEC_PROGRAM(${ITK_SOURCE_DIR}/Code/Numerics/vxl/configure 
                 ${ITK_BINARY_DIR}/Code/Numerics/vxl)
    EXEC_PROGRAM(${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/configure 
                 ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl)
  ENDIF(UNIX)
 ENDIF(NOT VXL_FROM_UTILITIES)
ENDIF(RUN_CONFIGURE)

# for win32 vc60, just copy files
IF(NOT UNIX)
 IF(NOT VXL_FROM_UTILITIES)
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
 ENDIF(NOT VXL_FROM_UTILITIES)
ENDIF(NOT UNIX)

# Add an option to use or not use VTK
OPTION(USE_VTK "Use VTK (The Visualization Toolkit) (some applications need this)" 0)
IF(USE_VTK)
  INCLUDE (${CMAKE_ROOT}/Modules/FindVTK.cmake)
  IF (USE_VTK_FILE)
    INCLUDE (${USE_VTK_FILE})
      SET(HAS_VTK 1)
    # Test for VTK using std libs
    IF(NOT VTK_USE_ANSI_STDLIB)
      MESSAGE(
	"Warning. Your VTK was not built with the VTK_USE_ANSI_STDLIB option ON.\n"
        "Link errors may occur.  Consider re-building VTK with this option ON.\n"
        "For MSVC 6, you MUST turn on the VTK option.  ITK will not link otherwise.")
    ENDIF(NOT VTK_USE_ANSI_STDLIB)
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
ELSE(USE_FLTK)
  SET (HAS_FLTK 0)
ENDIF(USE_FLTK)

# Copy the UseITK.cmake file to the binary tree for backward compatability.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake.in
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Help other projects use ITK.

# Save the compiler settings so another project can import them.
IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
  INCLUDE(${CMAKE_ROOT}/Modules/CMakeExportBuildSettings.cmake)
  CMAKE_EXPORT_BUILD_SETTINGS(${ITK_BINARY_DIR}/ITKBuildSettings.cmake)
  INSTALL_FILES(/lib/InsightToolkit .cmake ITKBuildSettings)
ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)

# Create the ITKConfig.cmake file containing the ITK configuration.
INCLUDE (${ITK_SOURCE_DIR}/itkGenerateITKConfig.cmake)

# Install some files.
INSTALL_FILES(/include/InsightToolkit .h itkConfigure)
INSTALL_FILES(/lib/InsightToolkit .cmake UseITK ITKLibraryDepends)

#-----------------------------------------------------------------------------
# Add compiler flags ITK needs to work on this platform.  This must be
# done AFTER the call to CMAKE_EXPORT_BUILD_SETTINGS.

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
