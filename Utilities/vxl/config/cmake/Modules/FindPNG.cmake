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
IF( VXL_FORCE_V3P_PNG )
ELSE( VXL_FORCE_V3P_PNG )
  INCLUDE( ${CMAKE_ROOT}/Modules/FindPNG.cmake )
ENDIF( VXL_FORCE_V3P_PNG )

IF(PNG_FOUND)

  SET(VXL_USING_NATIVE_PNG "YES")

ELSE(PNG_FOUND)

  INCLUDE( ${MODULE_PATH}/FindZLIB.cmake ) 
  IF(ZLIB_FOUND)

  #
  # At some point, in a "release" version, it is possible that someone
  # will not have the v3p png library, so make sure the headers
  # exist.
  #
  

    IF(EXISTS ${vxl_SOURCE_DIR}/v3p/png/png.h)

      SET( PNG_FOUND "YES" )
      SET( PNG_LIBRARIES png)
      SET( PNG_INCLUDE_DIR ${vxl_SOURCE_DIR}/v3p/png ${ZLIB_INCLUDE_DIR} )

      IF (CYGWIN)
        IF(BUILD_SHARED_LIBS)
           # No need to define PNG_USE_DLL here, because it's default for Cygwin.
        ELSE(BUILD_SHARED_LIBS)
          SET (PNG_DEFINITIONS  ${PNG_DEFINITIONS} -DPNG_STATIC)
        ENDIF(BUILD_SHARED_LIBS)
      ENDIF (CYGWIN)

    ENDIF(EXISTS ${vxl_SOURCE_DIR}/v3p/png/png.h)

  ENDIF(ZLIB_FOUND)
ENDIF(PNG_FOUND)
