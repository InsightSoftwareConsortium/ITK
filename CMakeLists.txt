# This is the root ITK CMakeLists file.

PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c).*$")

BUILD_SHARED_LIBRARIES()

SOURCE_GROUP("XML Files" "\.xml$")

# Include standard Testing module
INCLUDE (${CMAKE_ROOT}/Modules/Testing.cmake)

#
# Variables used for submitting testing results. These will change from
# project to project. Most likely, you will change DROP_SITE, 
# DROP_SITE_PASSWORD and TRIGGER_SITE.
# 
IF (BUILD_TESTING)
  SUBDIRS(TESTING)

  SET (DROP_SITE "public.kitware.com")
  SET (DROP_LOCATION "/incoming")
  SET (DROP_SITE_USER "anonymous")
  SET (DROP_SITE_PASSWORD "insight-tester@somewhere.com")
  SET (TRIGGER_SITE "http://${DROP_SITE}/cgi-bin/Submit-Insight-TestingResults.pl")

  SET (CVS_WEB_URL "http://${DROP_SITE}/cgi-bin/itkcvsweb.cgi/Insight/")
  SET (DOXYGEN_URL "http://${DROP_SITE}/Insight/Doxygen/html/")
  SET (GNATS_WEB_URL "http://${DROP_SITE}/cgi-bin/gnatsweb.pl/Insight/")
ENDIF (BUILD_TESTING)


# set up subdirectories
SUBDIRS(Utilities Code)
# Add an option to build or not build the Unsupported stuff
OPTION(BUILD_UNSUPPORTED "Build the unsupported directory")
IF(BUILD_UNSUPPORTED)
SUBDIRS(Unsupported)
ENDIF(BUILD_UNSUPPORTED)

# set project include directories
INCLUDE_DIRECTORIES(
${ITK_BINARY_DIR} 
${ITK_SOURCE_DIR}/Code/Numerics
${ITK_SOURCE_DIR}/Code/Common 
${ITK_BINARY_DIR}/Code/Numerics/vxl 
${ITK_SOURCE_DIR}/Code/Numerics/vxl 
${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl 
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl 
)

# set link libraries
LINK_DIRECTORIES(
${ITK_BINARY_DIR}/Code/Common 
${ITK_BINARY_DIR}/Code/Numerics/vxl )

# set link libraries for project
IF(UNIX)
  LINK_LIBRARIES(${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS})
ENDIF(UNIX)


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
    CONFIGURE_FILE_NOAUTOCONF(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vxl_config.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vxl_config.h)
    CONFIGURE_FILE_NOAUTOCONF(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_compiler.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_compiler.h)
    CONFIGURE_FILE_NOAUTOCONF(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_headers.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_headers.h)
    CONFIGURE_FILE_NOAUTOCONF(
        ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_manual.h
        ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_manual.h)
  ENDIF(WIN32)
ENDIF(NOT UNIX)

# Find the CABLE utility.
UTILITY_SOURCE(CABLE cable Utilities/Cable/Executables)
FIND_PROGRAM(CABLE cable)

