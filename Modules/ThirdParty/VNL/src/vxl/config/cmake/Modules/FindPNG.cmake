#
# Find a PNG library
#
#
# This file is used to manage using either a natively provided PNG library or the one in v3p if provided.
#
#
# As per the standard scheme the following definitions are used
# PNG_INCLUDE_DIR - where to find png.h
# PNG_LIBRARIES   - the set of libraries to include to use PNG.
# PNG_DEFINITIONS - You should ADD_DEFINITONS(${PNG_DEFINITIONS}) before compiling code that includes png library files.
# PNG_FOUND       - TRUE, if available somewhere on the system.

# Additionally
# VXL_USING_NATIVE_PNG  - True if we are using a PNG library provided outsidevxl (or v3p)


# If this FORCE variable is unset or is FALSE, try to find a native library.
if( VXL_FORCE_V3P_PNG )
else()
# Suppress not found messages
  set( ZLIB_FIND_QUIETLY "YES" )
  find_package( PNG QUIET )
  set( ZLIB_FIND_QUIETLY )
endif()

if(PNG_FOUND)

  set(VXL_USING_NATIVE_PNG "YES")

else()

  include( ${MODULE_PATH}/FindZLIB.cmake )
  if(ZLIB_FOUND)

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p png library, so make sure the headers
  # exist.
  #


    if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/png/png.h)

      set( PNG_FOUND "YES" )
      set( PNG_LIBRARIES png)
      set( PNG_INCLUDE_DIR ${VXL_ROOT_SOURCE_DIR}/v3p/png ${ZLIB_INCLUDE_DIR} )
      set( PNG_INSTALL_INCLUDE_DIR
        ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p/png
        ${ZLIB_INSTALL_INCLUDE_DIR}
      )

      if(CYGWIN)
        if(VXL_BUILD_SHARED_LIBS)
           # No need to define PNG_USE_DLL here, because it's default for Cygwin.
        else()
          set(PNG_DEFINITIONS  ${PNG_DEFINITIONS} -DPNG_STATIC)
        endif()
      endif()

    endif()

  endif()
endif()
