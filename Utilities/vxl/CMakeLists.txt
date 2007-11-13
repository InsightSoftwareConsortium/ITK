# Root vxl ported to ITK Utilities.
PROJECT(vxl)

INCLUDE_REGULAR_EXPRESSION("^.*$")

IF(NOT ITK_SOURCE_DIR)
  SET(BUILD_TESTING 1)
  SET(LIBRARY_OUTPUT_PATH ${vxl_BINARY_DIR}/bin)
  SET(EXECUTABLE_OUTPUT_PATH ${vxl_BINARY_DIR}/bin)
ENDIF(NOT ITK_SOURCE_DIR)

# CMake support directory.
INCLUDE(${vxl_SOURCE_DIR}/config/cmake/config/vxl_utils.cmake)
SET(VXL_CMAKE_DIR ${vxl_SOURCE_DIR}/config/cmake/Modules)
SET(MODULE_PATH ${vxl_SOURCE_DIR}/config/cmake/Modules)

# Do platform-specific configuration.
SUBDIRS(config/cmake/config)

# Build the core vxl + support libraries
SUBDIRS(vcl v3p core)

# Standard include directories.
SET(VXLCORE_INCLUDE_DIR ${vxl_BINARY_DIR}/core ${vxl_SOURCE_DIR}/core)
SET(VXLCORE_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/core)
SET(VCL_INCLUDE_DIR ${vxl_BINARY_DIR}/vcl ${vxl_SOURCE_DIR}/vcl)
SET(VCL_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/vcl)

INCLUDE_DIRECTORIES(${VCL_INCLUDE_DIR} ${VXLCORE_INCLUDE_DIR})

SET(VXL_INSTALL_ROOT ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl)
SET(VXL_INSTALL_LIB_DIR ${ITK_INSTALL_LIB_DIR})
SET(VXL_INSTALL_BIN_DIR ${ITK_INSTALL_BIN_DIR})
SET(VXL_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT})
SET(VXL_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES})

STRING(REGEX REPLACE "^/" "" VXL_INSTALL_LIB_DIR_CM24 "${VXL_INSTALL_LIB_DIR}")
STRING(REGEX REPLACE "^/" "" VXL_INSTALL_BIN_DIR_CM24 "${VXL_INSTALL_BIN_DIR}")

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
  
