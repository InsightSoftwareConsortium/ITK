#
# Find a CLIPPER library
#
# This file is used to manage using either a natively provided CLIPPER library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# CLIPPER_INCLUDE_DIR - where to find clipper.hxx
# CLIPPER_LIBRARIES   - the set of libraries to include to use CLIPPER.
# CLIPPER_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_CLIPPER  - True if we are using a CLIPPER library provided outside vxl (or v3p)

if( NOT CLIPPER_FOUND )

  # If this FORCE variable is unset or is FALSE, try to find a native library.
  if( NOT VXL_FORCE_V3P_CLIPPER )
    # ./FindGEOTIFF.cmake does this instead...
    #include( ${MODULE_PATH}/NewCMake/FindGEOTIFF.cmake )
    find_package( CLIPPER QUIET )
    if( CLIPPER_FOUND )
      set(VXL_USING_NATIVE_CLIPPER "YES")
    endif()
  endif()

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p clipper library, so make sure the headers
  # exist.
  #
  if( NOT CLIPPER_FOUND )
    if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/clipper/clipper.hxx)
      set( CLIPPER_FOUND "YES" )
      set( CLIPPER_INCLUDE_DIR ${clipper_BINARY_DIR} ${clipper_SOURCE_DIR})
      set( CLIPPER_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_DIR}/include/vxl/v3p/clipper)
      set( CLIPPER_LIBRARIES clipper )
    endif()
  endif()

endif()
