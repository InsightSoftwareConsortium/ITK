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

if (${VXL_USE_GEOTIFF})

  if( NOT GEOTIFF_FOUND )

    # If this FORCE variable is unset or is FALSE, try to find a native library.
    if( NOT VXL_FORCE_V3P_GEOTIFF )
      include( ${MODULE_PATH}/NewCMake/FindGEOTIFF.cmake )
      if( GEOTIFF_FOUND )
        set(VXL_USING_NATIVE_GEOTIFF "YES")
      endif()
    endif()

    #
    # At some point, in a "release" version, it is possible that someone
    # will not have the v3p geotiff library, so make sure the headers
    # exist.
    #
    if( NOT GEOTIFF_FOUND )
      if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/geotiff/geotiff.h)
        # Use FIND_PATH here to allow the user to set the path to IGNORE
        # to disable geotiff support.
        find_path(GEOTIFF_INCLUDE_DIR geotiff.h
          ${VXL_ROOT_SOURCE_DIR}/v3p/geotiff
        )
        if( GEOTIFF_INCLUDE_DIR )
          set( GEOTIFF_FOUND "YES" )
          set( GEOTIFF_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_DIR}/include/vxl/v3p/geotiff)
          set( GEOTIFF_LIBRARIES geotiff )
        endif()

      endif()
    endif()
  endif()
endif()
