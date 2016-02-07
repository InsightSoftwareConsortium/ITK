#
# Find an MPEG2 library
#
# This file is used to manage using either a natively provided MPEG2
# library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# MPEG2_INCLUDE_DIR - where to find mpeg2dec/mpeg2.h
# MPEG2_LIBRARIES   - the set of libraries to include to use MPEG2.
# MPEG2_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_MPEG2  - True if we are using a MPEG2 library provided outside vxl (or v3p)

# If this FORCE variable is unset or is FALSE, try to find a native library.
if( VXL_FORCE_V3P_MPEG2 )
else()
  find_package( MPEG2 QUIET )
endif()

if( MPEG2_FOUND )

  set(VXL_USING_NATIVE_MPEG2 "YES")

else()

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p mpeg2 library
  #

  if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/mpeg2/include/mpeg2dec/mpeg2.h)

    set( MPEG2_FOUND "YES" )
    set( MPEG2_LIBRARIES mpeg2 vo )
    set( MPEG2_INCLUDE_DIR
      ${VXL_ROOT_SOURCE_DIR}/v3p/mpeg2/include
      # use of the following is deprecated
      # it is better to use #include <mpeg2dec/mpeg2.h> in client code
      ${VXL_ROOT_SOURCE_DIR}/v3p/mpeg2/include/mpeg2dec
    )
    set( MPEG2_INSTALL_INCLUDE_DIR
      ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p/mpeg2/include
      # use of the following is deprecated
      # it is better to use #include <mpeg2dec/mpeg2.h> in client code
      ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p/mpeg2/include/mpeg2dec
    )

  endif()

endif()
