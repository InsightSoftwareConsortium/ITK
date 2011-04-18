if(NOT ITK_BUILD_ALL_MODULES)
  message(FATAL_ERROR "BUILD_EXAMPLES requires ITK_BUILD_ALL_MODULES")
endif()

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

macro(itk_add_test)
endmacro()

set(ITK_TEST_DRIVER itkTestDriver)

add_subdirectory(Iterators)
add_subdirectory(DataRepresentation)
add_subdirectory(Filtering)
add_subdirectory(IO)
add_subdirectory(Numerics)
add_subdirectory(Registration)
add_subdirectory(Segmentation)
add_subdirectory(SpatialObjects)
add_subdirectory(Statistics)

#This code looks like it needs to be updated to the new statistics framework
#It was never updated as part of the refactoring to review statistics.
#TMP_RMV20100801 add_subdirectory( Statistics )

if( ITK_USE_REVIEW )
  add_subdirectory( Review )
endif( ITK_USE_REVIEW )
