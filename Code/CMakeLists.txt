SUBDIRS(Numerics Common BasicFilters Algorithms)

LINK_LIBRARIES (
VXLNumerics
)

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

