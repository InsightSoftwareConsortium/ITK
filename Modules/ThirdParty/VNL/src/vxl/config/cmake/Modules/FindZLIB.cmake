#
# Find a ZLIB library
#
# This file is used to manage using either a natively provided ZLIB library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# ZLIB_INCLUDE_DIR - where to find zlib.h
# ZLIB_LIBRARIES   - the set of libraries to include to use ZLIB.
# ZLIB_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_ZLIB  - True if we are using a ZLIB library provided outside vxl (or v3p)


# If this FORCE variable is unset or is FALSE, try to find a native library.
if( VXL_FORCE_V3P_ZLIB )
else()
  # Suppress not found messages
  find_package( ZLIB QUIET )
endif()


if(ZLIB_FOUND)

  set(VXL_USING_NATIVE_ZLIB "YES")
  # All the other variables are set by CMake's FindZLIB. Don't
  # set them here.

else()

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p ZLIB library, so make sure the headers
  # exist.
  #

  if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/zlib/zlib.h)

    set( ZLIB_FOUND "YES" )
    set( ZLIB_INCLUDE_DIR ${VXL_ROOT_SOURCE_DIR}/v3p/zlib)
    set( ZLIB_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p/zlib)
    set( ZLIB_LIBRARIES z )

  endif()
endif()
