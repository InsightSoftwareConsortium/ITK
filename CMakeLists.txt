# This is the root ITK CMakeLists file.

PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c).*$")

BUILD_SHARED_LIBRARIES()

SOURCE_GROUP("XML Files" "\.xml$")

# Include the include and link libraries
INCLUDE (${ITK_SOURCE_DIR}/itkCMakeOptions.cmake)

# Include the standard Dart testing module
INCLUDE (${CMAKE_ROOT}/Modules/Dart.cmake)

# Walk into these directories before Testing or Unsupported.
SUBDIRS(Utilities Code)

#
# Variables used for submitting testing results. These will change from
# project to project. Most likely, you will change DROP_SITE, 
# DROP_SITE_PASSWORD and TRIGGER_SITE.
# 
IF (BUILD_TESTING)
  ENABLE_TESTING()
  SUBDIRS(Testing)

  SET (DROP_SITE "public.kitware.com" CACHE INTERNAL "FTP site for submitting testing results")
  SET (DROP_LOCATION "/incoming" CACHE INTERNAL "Directory on FTP site to place testing results")
  SET (DROP_SITE_USER "anonymous" CACHE INTERNAL "Username for FTP site for submitting testing results")
  SET (DROP_SITE_PASSWORD "insight-tester@somewhere.com" CACHE INTERNAL "Password for FTP site for submitting testing results")
  SET (TRIGGER_SITE "http://${DROP_SITE}/cgi-bin/Submit-Insight-TestingResults.pl" CACHE INTERNAL "Web site to signal when testing results have been submitted")

  SET (CVS_WEB_URL "http://${DROP_SITE}/cgi-bin/itkcvsweb.cgi/Insight/" CACHE INTERNAL "URL for revision control system")
  SET (DOXYGEN_URL "http://${DROP_SITE}/Insight/Doxygen/html/" CACHE INTERNAL "URL for source code documentation")
  SET (GNATS_WEB_URL "http://${DROP_SITE}/cgi-bin/gnatsweb.pl/Insight/" CACHE INTERNAL "URL for bug tracker")
ENDIF (BUILD_TESTING)


# Add an option to build or not build the Examples
OPTION(BUILD_EXAMPLES "Build the examples directory")
IF(BUILD_EXAMPLES)
SUBDIRS(Examples)
ENDIF(BUILD_EXAMPLES)


# Add an option to build or not build the Unsupported stuff
OPTION(BUILD_UNSUPPORTED "Build the unsupported directory")
IF(BUILD_UNSUPPORTED)
SUBDIRS(Unsupported)
ENDIF(BUILD_UNSUPPORTED)

# Configure files
CONFIGURE_FILE(
${ITK_SOURCE_DIR}/itkConfigure.h.in 
${ITK_BINARY_DIR}/itkConfigure.h)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Code/doxygen.config.in
${ITK_BINARY_DIR}/Code/doxygen.config)

# make directories in the binary tree
MAKE_DIRECTORY(${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl)

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

# Find the CABLE utility.
UTILITY_SOURCE(CABLE cable Utilities/Cable/Executables)
FIND_PROGRAM(CABLE cable)

