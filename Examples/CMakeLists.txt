add_subdirectory(Iterators)
add_subdirectory(DataRepresentation)
add_subdirectory(Filtering)
add_subdirectory(IO)
add_subdirectory(Numerics)
add_subdirectory(Registration)
add_subdirectory(Segmentation)
add_subdirectory(SpatialObjects)

#This code looks like it needs to be updated to the new statistics framework
#It was never updated as part of the refactoring to review statistics.
#TMP_RMV20100801 add_subdirectory( Statistics )

if( ITK_USE_REVIEW )
  add_subdirectory( Review )
endif( ITK_USE_REVIEW )

if( ITK_USE_PATENTED )
  add_subdirectory( Patented )
endif( ITK_USE_PATENTED )

