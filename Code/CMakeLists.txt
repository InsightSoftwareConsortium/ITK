SUBDIRS(Numerics Common BasicFilters IO Algorithms)

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

