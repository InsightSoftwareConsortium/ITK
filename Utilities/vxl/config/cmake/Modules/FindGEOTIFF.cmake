#
# Find a GEOTIFF library
#
# This file is used to manage using either a natively provided GEOTIFF library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# GEOTIFF_INCLUDE_DIR - where to find geotiff.h
# GEOTIFF_LIBRARIES   - the set of libraries to include to use GEOTIFF.
# GEOTIFF_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_GEOTIFF  - True if we are using a GEOTIFF library provided outside vxl (or v3p)


# If this FORCE variable is unset or is FALSE, try to find a native library.
IF( VXL_FORCE_V3P_GEOTIFF )
ELSE( VXL_FORCE_V3P_GEOTIFF )
  INCLUDE( ${MODULE_PATH}/NewCMake/FindGEOTIFF.cmake )
ENDIF( VXL_FORCE_V3P_GEOTIFF )

  
IF(GEOTIFF_FOUND)

  SET(VXL_USING_NATIVE_GEOTIFF "YES")

ELSE(GEOTIFF_FOUND)

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p geotiff library, so make sure the headers
  # exist.
  #
  
  IF(EXISTS ${vxl_SOURCE_DIR}/v3p/geotiff/geotiff.h)

    SET( GEOTIFF_FOUND "YES" )
    SET( GEOTIFF_INCLUDE_DIR ${vxl_SOURCE_DIR}/v3p/geotiff)  
    SET( GEOTIFF_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_DIR}/include/vxl/v3p/geotiff)
    SET( GEOTIFF_LIBRARIES geotiff )
  
  ENDIF(EXISTS ${vxl_SOURCE_DIR}/v3p/geotiff/geotiff.h)
  
ENDIF(GEOTIFF_FOUND)
