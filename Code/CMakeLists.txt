add_subdirectory(Common)
add_subdirectory(Numerics)
add_subdirectory(BasicFilters)
add_subdirectory(Algorithms)
add_subdirectory(SpatialObject)

if( ITK_USE_PATENTED )
  add_subdirectory( Patented )
endif( ITK_USE_PATENTED )

if( ITK_USE_REVIEW )
  add_subdirectory( Review )
endif( ITK_USE_REVIEW )

## There should be no library dependances on IO, but IO may depend on other
## libraries, so build it last
add_subdirectory(IO)
