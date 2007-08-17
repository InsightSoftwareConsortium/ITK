SUBDIRS(Common Numerics BasicFilters Algorithms SpatialObject)

IF( ITK_USE_PATENTED )
  SUBDIRS( Patented )
ENDIF( ITK_USE_PATENTED )

IF( ITK_USE_REVIEW )
  SUBDIRS( Review )
ENDIF( ITK_USE_REVIEW )

## There should be no library dependances on IO, but IO may depend on other
## libraries, so build it last
SUBDIRS(IO)
