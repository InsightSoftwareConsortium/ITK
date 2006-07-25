# Root vxl ported to ITK Utilities.
PROJECT(vxl)

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
SET(VCL_INCLUDE_DIR ${vxl_BINARY_DIR}/vcl ${vxl_SOURCE_DIR}/vcl)

INCLUDE_DIRECTORIES(${VCL_INCLUDE_DIR} ${VXLCORE_INCLUDE_DIR})

SET(VXL_INSTALL_ROOT ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl)
SET(VXL_INSTALL_LIB_DIR ${ITK_INSTALL_LIB_DIR})
SET(VXL_INSTALL_BIN_DIR ${ITK_INSTALL_BIN_DIR})
SET(VXL_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT})
SET(VXL_INSTALL_NO_LIBRARIES ${ITK_INSTALL_NO_LIBRARIES})
