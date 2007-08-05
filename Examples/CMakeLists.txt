SUBDIRS( 
  Iterators
  DataRepresentation
  Filtering
  IO
  Numerics
  Registration
  Segmentation
  SpatialObjects
  Statistics
)

IF( ITK_USE_PATENTED )
  SUBDIRS( Patented )
ENDIF( ITK_USE_PATENTED )

