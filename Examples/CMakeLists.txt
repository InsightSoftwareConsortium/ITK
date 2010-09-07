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

#This code looks like it needs to be updated to the new statistics framework
#It was never updated as part of the refactoring to review statistics.
#TMP_RMV20100801 SUBDIRS( Statistics )

IF( ITK_USE_REVIEW )
  SUBDIRS( Review )
ENDIF( ITK_USE_REVIEW )

IF( ITK_USE_PATENTED )
  SUBDIRS( Patented )
ENDIF( ITK_USE_PATENTED )

