SUBDIRS(Numerics Common BasicFilters Algorithms)

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

