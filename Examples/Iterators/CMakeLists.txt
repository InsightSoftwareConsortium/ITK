#
#  Examples on the use of Iterators.
#
project(IteratorExamples)
include_regular_expression("^.*$")

add_executable(NeighborhoodIterators4 NeighborhoodIterators4.cxx)
target_link_libraries(NeighborhoodIterators4 ${ITK_LIBRARIES})

add_executable(NeighborhoodIterators5 NeighborhoodIterators5.cxx)
target_link_libraries(NeighborhoodIterators5 ${ITK_LIBRARIES})

add_executable(ImageRandomConstIteratorWithIndex ImageRandomConstIteratorWithIndex.cxx)
target_link_libraries(ImageRandomConstIteratorWithIndex ${ITK_LIBRARIES})

add_executable(NeighborhoodIterators1 NeighborhoodIterators1.cxx)
target_link_libraries(NeighborhoodIterators1 ${ITK_LIBRARIES})

add_executable(NeighborhoodIterators2 NeighborhoodIterators2.cxx)
target_link_libraries(NeighborhoodIterators2 ${ITK_LIBRARIES})

add_executable(NeighborhoodIterators3 NeighborhoodIterators3.cxx)
target_link_libraries(NeighborhoodIterators3 ${ITK_LIBRARIES})

add_executable(NeighborhoodIterators6 NeighborhoodIterators6.cxx)
target_link_libraries(NeighborhoodIterators6 ${ITK_LIBRARIES})

add_executable(ImageRegionIteratorWithIndex ImageRegionIteratorWithIndex.cxx)
target_link_libraries(ImageRegionIteratorWithIndex ${ITK_LIBRARIES})

add_executable(ImageRegionIterator ImageRegionIterator.cxx)
target_link_libraries(ImageRegionIterator ${ITK_LIBRARIES})

add_executable(ImageLinearIteratorWithIndex2 ImageLinearIteratorWithIndex2.cxx)
target_link_libraries(ImageLinearIteratorWithIndex2 ${ITK_LIBRARIES})

add_executable(ImageLinearIteratorWithIndex ImageLinearIteratorWithIndex.cxx)
target_link_libraries(ImageLinearIteratorWithIndex ${ITK_LIBRARIES})

add_executable(ImageSliceIteratorWithIndex ImageSliceIteratorWithIndex.cxx)
target_link_libraries(ImageSliceIteratorWithIndex ${ITK_LIBRARIES})

add_executable(ShapedNeighborhoodIterators1 ShapedNeighborhoodIterators1.cxx)
target_link_libraries(ShapedNeighborhoodIterators1 ${ITK_LIBRARIES})

add_executable(ShapedNeighborhoodIterators2 ShapedNeighborhoodIterators2.cxx)
target_link_libraries(ShapedNeighborhoodIterators2 ${ITK_LIBRARIES})

add_executable(IteratorsExamples IteratorsExamples.cxx )
target_link_libraries(IteratorsExamples ${ITK_LIBRARIES})

set(ITERATORS_EXAMPLES ${CXX_TEST_PATH}/IteratorsExamples)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Iterators)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

if(BUILD_TESTING)

add_test(ImageLinearIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageLinearIteratorWithIndexTest.png
            ${TEMP}/ImageLinearIteratorWithIndexTest.png
  ImageLinearIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ImageLinearIteratorWithIndexTest.png
)

add_test(ImageRegionIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageRegionIteratorWithIndexTest.png
            ${TEMP}/ImageRegionIteratorWithIndexTest.png
  ImageRegionIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ImageRegionIteratorWithIndexTest.png
)

add_test(ImageRegionIteratorTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageRegionIteratorTest.png
            ${TEMP}/ImageRegionIteratorTest.png
  ImageRegionIteratorTest
        ${ITK_SOURCE_DIR}/Examples/Data/FatMRISlice.png
        ${TEMP}/ImageRegionIteratorTest.png
        20 70 210 140
)

add_test(ImageSliceIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageSliceIteratorWithIndexTest.mha
            ${TEMP}/ImageSliceIteratorWithIndexTest.mha
  ImageSliceIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensity3Slices.mha
        ${TEMP}/ImageSliceIteratorWithIndexTest.mha
        2
)
endif(BUILD_TESTING)
