SUBDIRS(Numerics Common BasicFilters IO Algorithms SpatialObject )

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

