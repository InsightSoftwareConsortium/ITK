#
# Find a JPEG library
#
#
# This file is used to manage using either a natively provided JPEG library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# JPEG_INCLUDE_DIR - where to find jpeglib.h
# JPEG_LIBRARIES   - the set of libraries to include to use JPEG.
# JPEG_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_JPEG  - True if we are using a JPEG library provided outside vxl (or v3p)

# If this FORCE variable is unset or is FALSE, try to find a native library.
IF( VXL_FORCE_V3P_JPEG )
ELSE( VXL_FORCE_V3P_JPEG )
  INCLUDE( ${CMAKE_ROOT}/Modules/FindJPEG.cmake )
ENDIF( VXL_FORCE_V3P_JPEG )

IF(JPEG_FOUND)

  SET(VXL_USING_NATIVE_JPEG "YES")

ELSE(JPEG_FOUND)

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p jpeg library
  #

  IF(EXISTS ${vxl_SOURCE_DIR}/v3p/jpeg/jpeglib.h)

    SET( JPEG_FOUND "YES" )
    SET( JPEG_LIBRARIES jpeg )  
    SET( JPEG_INCLUDE_DIR ${vxl_SOURCE_DIR}/v3p/jpeg)  
	
  ENDIF(EXISTS ${vxl_SOURCE_DIR}/v3p/jpeg/jpeglib.h)

ENDIF(JPEG_FOUND)
