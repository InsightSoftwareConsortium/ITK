SUBDIRS(Numerics Common BasicFilters IO Algorithms)
SUBDIR_DEPENDS(Common Numerics)
SUBDIR_DEPENDS(IO Common)
SUBDIR_DEPENDS(BasicFilters Common IO)
SUBDIR_DEPENDS(Algorithms BasicFilters)

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

