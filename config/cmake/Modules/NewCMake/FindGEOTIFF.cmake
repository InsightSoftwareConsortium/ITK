# - Find GEOTIFF library
# Find the native GEOTIFF includes and library
# This module defines
#  GEOTIFF_INCLUDE_DIR, where to find tiff.h, etc.
#  GEOTIFF_LIBRARIES, libraries to link against to use GEOTIFF.
#  GEOTIFF_FOUND, If false, do not try to use GEOTIFF.
# also defined, but not for general use are
#  GEOTIFF_LIBRARY, where to find the GEOTIFF library.

find_path(GEOTIFF_INCLUDE_DIR geotiff.h
  PATH_SUFFIXES geotiff
  PATHS
    /usr/local/include
    /usr/include
)

set(GEOTIFF_NAMES ${GEOTIFF_NAMES} geotiff)
find_library(GEOTIFF_LIBRARY
  NAMES ${GEOTIFF_NAMES}
  PATHS /usr/lib /usr/local/lib
  )

set( GEOTIFF_FOUND "NO" )
if(GEOTIFF_INCLUDE_DIR)
  if(GEOTIFF_LIBRARY)
    set( GEOTIFF_FOUND "YES" )
    set( GEOTIFF_LIBRARIES ${GEOTIFF_LIBRARY} )
  endif()
endif()

