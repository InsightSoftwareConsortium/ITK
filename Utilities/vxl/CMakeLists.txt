PROJECT(vxl)
#
# Root vxl
#
#   geoff.cross@crd.ge.com

# VXL can handle this statement, which introduces warnings about deprecations.
# However, it is commented out for the benefit of those building other projects
# within the VXL tree, but not fully conforming to CMake 1.4 standards.
# After a waiting period, this line should be uncommented.
# CMAKE_MINIMUM_REQUIRED(VERSION 1.4)

PROJECT(vxl)
SET(BUILD_TESTING 1)
SET(BUILD_EXAMPLES 1)
# To avoid typing for the FindXXX.cmake modules
SET( MODULE_PATH ${vxl_SOURCE_DIR}/config/cmake/Modules CACHE INTERNAL "VXL module path" )

INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/vxl/vcl
                    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
                    ${ITK_SOURCE_DIR}/Utilities/vxl)

IF(WIN32)
  IF(NOT CYGWIN)
    INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/vxl/vcl/config.win32)
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)


INCLUDE(${MODULE_PATH}/FindNetlib.cmake)
# Build the core vxl + support libraries
SUBDIRS(v3p/netlib testlib vnl)

IF(RUN_CONFIGURE)
  EXEC_PROGRAM( ${vxl_SOURCE_DIR}/configure
                ${vxl_BINARY_DIR} )
ENDIF(RUN_CONFIGURE)

# Some types of path names can cause havoc with regular expressions,
# so avoid those.
IF( ${vxl_BINARY_DIR} MATCHES  \\+ )
  MESSAGE(SEND_ERROR "You cannot have a + in your binary path")
ENDIF( ${vxl_BINARY_DIR} MATCHES  \\+ )
IF( ${vxl_SOURCE_DIR} MATCHES  \\+ )
  MESSAGE(SEND_ERROR "You cannot have a + in your source path")
ENDIF( ${vxl_SOURCE_DIR} MATCHES  \\+ )
