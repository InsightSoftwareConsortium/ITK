project(IteratorExamples)

add_executable(NeighborhoodIterators4 NeighborhoodIterators4.cxx)
target_link_libraries(
  NeighborhoodIterators4
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(NeighborhoodIterators5 NeighborhoodIterators5.cxx)
target_link_libraries(
  NeighborhoodIterators5
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(
  ImageRandomConstIteratorWithIndex
  ImageRandomConstIteratorWithIndex.cxx
)
target_link_libraries(
  ImageRandomConstIteratorWithIndex
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(NeighborhoodIterators1 NeighborhoodIterators1.cxx)
target_link_libraries(
  NeighborhoodIterators1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(NeighborhoodIterators2 NeighborhoodIterators2.cxx)
target_link_libraries(
  NeighborhoodIterators2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(NeighborhoodIterators3 NeighborhoodIterators3.cxx)
target_link_libraries(
  NeighborhoodIterators3
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(NeighborhoodIterators6 NeighborhoodIterators6.cxx)
target_link_libraries(
  NeighborhoodIterators6
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageIntensityModule
    ITK::ITKTestKernelModule
)

add_executable(ImageRegionIteratorWithIndex ImageRegionIteratorWithIndex.cxx)
target_link_libraries(
  ImageRegionIteratorWithIndex
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageRegionIterator ImageRegionIterator.cxx)
target_link_libraries(
  ImageRegionIterator
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageLinearIteratorWithIndex2 ImageLinearIteratorWithIndex2.cxx)
target_link_libraries(
  ImageLinearIteratorWithIndex2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageLinearIteratorWithIndex ImageLinearIteratorWithIndex.cxx)
target_link_libraries(
  ImageLinearIteratorWithIndex
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageSliceIteratorWithIndex ImageSliceIteratorWithIndex.cxx)
target_link_libraries(
  ImageSliceIteratorWithIndex
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ShapedNeighborhoodIterators1 ShapedNeighborhoodIterators1.cxx)
target_link_libraries(
  ShapedNeighborhoodIterators1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ShapedNeighborhoodIterators2 ShapedNeighborhoodIterators2.cxx)
target_link_libraries(
  ShapedNeighborhoodIterators2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
