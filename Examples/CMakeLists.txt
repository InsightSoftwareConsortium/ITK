SUBDIRS( 
  Iterators
  DataRepresentation
  Filtering
  IO
  Numerics
  Registration
  Segmentation
  SpatialObjects
)

IF( ITK_USE_REVIEW )
  SUBDIRS( Review )
ENDIF( ITK_USE_REVIEW )

IF( NOT ITK_USE_REVIEW_STATISTICS )
  SUBDIRS( Statistics )
ENDIF( NOT ITK_USE_REVIEW_STATISTICS )

IF( ITK_USE_PATENTED )
  SUBDIRS( Patented )
ENDIF( ITK_USE_PATENTED )

