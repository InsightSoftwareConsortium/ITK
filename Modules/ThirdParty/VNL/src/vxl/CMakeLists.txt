# Root vxl ported to ITK Utilities.
PROJECT(vxl)

INCLUDE_REGULAR_EXPRESSION("^.*$")

# CMake support directory.
INCLUDE(${vxl_SOURCE_DIR}/config/cmake/config/vxl_utils.cmake)
SET(VXL_CMAKE_DIR ${vxl_SOURCE_DIR}/config/cmake/Modules)
SET(MODULE_PATH ${vxl_SOURCE_DIR}/config/cmake/Modules)

# Use this variable in vxl_config.h to tell if it is a shared build
set(VXL_BUILD_SHARED FALSE)
if(BUILD_SHARED_LIBS)
  set(VXL_BUILD_SHARED TRUE)
endif()

# Do platform-specific configuration.
SUBDIRS(config/cmake/config)

# Build the core vxl + support libraries
SUBDIRS(vcl v3p core)

# Standard include directories.
SET(VXLCORE_INCLUDE_DIR ${vxl_BINARY_DIR}/core ${vxl_SOURCE_DIR}/core)
SET(VCL_INCLUDE_DIR ${vxl_BINARY_DIR}/vcl ${vxl_SOURCE_DIR}/vcl)

INCLUDE_DIRECTORIES(${VCL_INCLUDE_DIR} ${VXLCORE_INCLUDE_DIR})

IF(NOT VXL_INSTALL_EXPORT_NAME)
  SET(VXL_INSTALL_EXPORT_NAME VXL-targets)
ENDIF()
IF(NOT VXL_INSTALL_RUNTIME_DIR)
  SET(VXL_INSTALL_RUNTIME_DIR bin)
ENDIF()
IF(NOT VXL_INSTALL_LIBRARY_DIR)
  SET(VXL_INSTALL_LIBRARY_DIR lib)
ENDIF()
IF(NOT VXL_INSTALL_ARCHIVE_DIR)
  SET(VXL_INSTALL_ARCHIVE_DIR lib)
ENDIF()
IF(NOT VXL_INSTALL_INCLUDE_DIR)
  SET(VXL_INSTALL_INCLUDE_DIR include)
ENDIF()

IF(WIN32)
  IF(NOT UNIX)
    IF(NOT BORLAND)
      IF(NOT MINGW)
        SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4244")
        # Revert: /wd8004 /wd8057 produces warnings on .Net2003:
        #         invalid value '8004' for '/wd'; assuming '4999
        # you probably do not want to disable 4999, do you?
        # Which compiler were 8004 and 8057 referring to?
      ENDIF(NOT MINGW)
    ENDIF(NOT BORLAND)
  ENDIF(NOT UNIX)
ENDIF(WIN32)

IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)
  
