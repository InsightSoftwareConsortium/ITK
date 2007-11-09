# - Find GEOTIFF library
# Find the native GEOTIFF includes and library
# This module defines
#  GEOTIFF_INCLUDE_DIR, where to find tiff.h, etc.
#  GEOTIFF_LIBRARIES, libraries to link against to use GEOTIFF.
#  GEOTIFF_FOUND, If false, do not try to use GEOTIFF.
# also defined, but not for general use are
#  GEOTIFF_LIBRARY, where to find the GEOTIFF library.

FIND_PATH(GEOTIFF_INCLUDE_DIR geotiff.h
  /usr/local/include
  /usr/include
)

SET(GEOTIFF_NAMES ${GEOTIFF_NAMES} geotiff)
FIND_LIBRARY(GEOTIFF_LIBRARY
  NAMES ${GEOTIFF_NAMES}
  PATHS /usr/lib /usr/local/lib
  )

IF(GEOTIFF_INCLUDE_DIR)
  IF(GEOTIFF_LIBRARY)
    SET( GEOTIFF_FOUND "YES" )
    SET( GEOTIFF_LIBRARIES ${GEOTIFF_LIBRARY} )
  ENDIF(GEOTIFF_LIBRARY)
ENDIF(GEOTIFF_INCLUDE_DIR)

