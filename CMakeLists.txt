# This is the root ITK CMakeLists file.

PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c).*$")

IF(NOT WIN32)
  OPTION(BUILD_SHARED_LIBS "Build with shared libraries." OFF)
ENDIF(NOT WIN32)

SOURCE_GROUP("XML Files" "[.]xml$")

# Include the include and link libraries
INCLUDE (${ITK_SOURCE_DIR}/itkCMakeOptions.cmake)

# Walk into these directories before Testing or Unsupported.
SUBDIRS(Utilities Code)
SUBDIR_DEPENDS(Code Utilities)

# Include the standard Dart testing module
INCLUDE (${CMAKE_ROOT}/Modules/Dart.cmake)
IF (BUILD_TESTING)
  SUBDIRS(Testing)
  SUBDIR_DEPENDS(Testing Code)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/InsightLogo.gif ${ITK_BINARY_DIR}/Testing/HTML/TestingResults/Icons/Logo.gif COPYONLY)
ENDIF (BUILD_TESTING)

CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY)

# Add an option to build or not build the Examples
OPTION(BUILD_EXAMPLES "Build the examples directory")
IF(BUILD_EXAMPLES)
  SUBDIRS(Examples)
  SUBDIR_DEPENDS(Examples Code)
ENDIF(BUILD_EXAMPLES)


# Add an option to build or not build the Unsupported stuff
OPTION(BUILD_UNSUPPORTED "Build the unsupported directory")
IF(BUILD_UNSUPPORTED)
  SUBDIRS(Unsupported)
  SUBDIR_DEPENDS(Unsupported Code)
ENDIF(BUILD_UNSUPPORTED)

# Configure files
CONFIGURE_FILE(
${ITK_SOURCE_DIR}/itkConfigure.h.in 
${ITK_BINARY_DIR}/itkConfigure.h)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/doxygen.config.in
${ITK_BINARY_DIR}/doxygen.config)

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

OPTION(ITK_WRAP_TCL "Build Tcl wrapper support." OFF)

IF(ITK_WRAP_TCL)
  SUBDIRS(Wrapping)
  SUBDIR_DEPENDS(Wrapping Code)
ENDIF(ITK_WRAP_TCL)
