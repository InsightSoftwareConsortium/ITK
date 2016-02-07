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
if( VXL_FORCE_V3P_JPEG )
else()
  find_package( JPEG QUIET )
endif()

if(JPEG_FOUND)

  set(VXL_USING_NATIVE_JPEG "YES")

else()

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p jpeg library
  #

  if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/jpeg/jpeglib.h)

    set( JPEG_FOUND "YES" )
    set( JPEG_LIBRARIES jpeg )
    set( JPEG_INCLUDE_DIR ${VXL_ROOT_SOURCE_DIR}/v3p/jpeg)
    set( JPEG_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p/jpeg)

  endif()

endif()
