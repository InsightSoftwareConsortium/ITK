project(IteratorExamples)

add_executable(NeighborhoodIterators4 NeighborhoodIterators4.cxx)
target_link_libraries(
  NeighborhoodIterators4
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(NeighborhoodIterators5 NeighborhoodIterators5.cxx)
target_link_libraries(
  NeighborhoodIterators5
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(
  ImageRandomConstIteratorWithIndex
  ImageRandomConstIteratorWithIndex.cxx
)
target_link_libraries(
  ImageRandomConstIteratorWithIndex
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(NeighborhoodIterators1 NeighborhoodIterators1.cxx)
target_link_libraries(
  NeighborhoodIterators1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(NeighborhoodIterators2 NeighborhoodIterators2.cxx)
target_link_libraries(
  NeighborhoodIterators2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(NeighborhoodIterators3 NeighborhoodIterators3.cxx)
target_link_libraries(
  NeighborhoodIterators3
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(NeighborhoodIterators6 NeighborhoodIterators6.cxx)
target_link_libraries(
  NeighborhoodIterators6
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKFastMarchingModule
    ITKImageIntensityModule
    ITKTestKernelModule
)

add_executable(ImageRegionIteratorWithIndex ImageRegionIteratorWithIndex.cxx)
target_link_libraries(
  ImageRegionIteratorWithIndex
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageRegionIterator ImageRegionIterator.cxx)
target_link_libraries(
  ImageRegionIterator
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageLinearIteratorWithIndex2 ImageLinearIteratorWithIndex2.cxx)
target_link_libraries(
  ImageLinearIteratorWithIndex2
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageLinearIteratorWithIndex ImageLinearIteratorWithIndex.cxx)
target_link_libraries(
  ImageLinearIteratorWithIndex
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageSliceIteratorWithIndex ImageSliceIteratorWithIndex.cxx)
target_link_libraries(
  ImageSliceIteratorWithIndex
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ShapedNeighborhoodIterators1 ShapedNeighborhoodIterators1.cxx)
target_link_libraries(
  ShapedNeighborhoodIterators1
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ShapedNeighborhoodIterators2 ShapedNeighborhoodIterators2.cxx)
target_link_libraries(
  ShapedNeighborhoodIterators2
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
