SUBDIRS(Common Numerics BasicFilters IO Algorithms SpatialObject)
IF( ITK_USE_PATENTED )
  SUBDIRS( Patented )
ENDIF( ITK_USE_PATENTED )
