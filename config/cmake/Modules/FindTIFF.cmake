#
# Find a TIFF library
#
# This file is used to manage using either a natively provided TIFF library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# TIFF_INCLUDE_DIR - where to find tiff.h
# TIFF_LIBRARIES   - the set of libraries to include to use TIFF.
# TIFF_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_TIFF  - True if we are using a TIFF library provided outside vxl (or v3p)

if( NOT TIFF_FOUND )

  # If this FORCE variable is unset or is FALSE, try to find a native library.
  if( NOT VXL_FORCE_V3P_TIFF )
    find_package( TIFF QUIET )
    if( TIFF_FOUND )
      set(VXL_USING_NATIVE_TIFF "YES")
    endif()
  endif()


  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p tiff library, so make sure the headers
  # exist.
  #

  if( NOT TIFF_FOUND )
    if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/tiff/tiff.h)

      set( TIFF_FOUND "YES" )
      set( TIFF_INCLUDE_DIR ${tiff_BINARY_DIR} ${tiff_SOURCE_DIR})
      set( TIFF_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_DIR}/include/vxl/v3p/tiff)
      set( TIFF_LIBRARIES tiff )

    endif()
  endif()

endif()
