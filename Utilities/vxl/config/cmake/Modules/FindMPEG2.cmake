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
IF( VXL_FORCE_V3P_MPEG2 )
ELSE( VXL_FORCE_V3P_MPEG2 )
  INCLUDE( ${CMAKE_ROOT}/Modules/FindMPEG2.cmake )
ENDIF( VXL_FORCE_V3P_MPEG2 )

IF( MPEG2_FOUND )

  SET(VXL_USING_NATIVE_MPEG2 "YES")

ELSE( MPEG2_FOUND )

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p mpeg2 library
  #

  IF(EXISTS ${vxl_SOURCE_DIR}/v3p/mpeg2/include/mpeg2dec/mpeg2.h)

    SET( MPEG2_FOUND "YES" )
    SET( MPEG2_LIBRARIES mpeg2 vo )
    SET( MPEG2_INCLUDE_DIR
      ${vxl_SOURCE_DIR}/v3p/mpeg2/include
      # use of the following is deprecated
      # it is better to use #include <mpeg2dec/mpeg2.h> in client code
      ${vxl_SOURCE_DIR}/v3p/mpeg2/include/mpeg2dec
    )

  ENDIF(EXISTS ${vxl_SOURCE_DIR}/v3p/mpeg2/include/mpeg2dec/mpeg2.h)

ENDIF( MPEG2_FOUND )
