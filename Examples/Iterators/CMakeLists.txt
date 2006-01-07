#
#  Examples on the use of Iterators.
#
PROJECT(IteratorExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(ImageRegionIterator ImageRegionIterator.cxx)
TARGET_LINK_LIBRARIES(ImageRegionIterator ITKIO   )

ADD_EXECUTABLE(ImageRegionIteratorWithIndex ImageRegionIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageRegionIteratorWithIndex ITKIO   )

ADD_EXECUTABLE(ImageLinearIteratorWithIndex ImageLinearIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageLinearIteratorWithIndex ITKIO   )

ADD_EXECUTABLE(ImageLinearIteratorWithIndex2 ImageLinearIteratorWithIndex2.cxx)
TARGET_LINK_LIBRARIES(ImageLinearIteratorWithIndex2 ITKIO   )

ADD_EXECUTABLE(ImageSliceIteratorWithIndex ImageSliceIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageSliceIteratorWithIndex ITKIO   )

ADD_EXECUTABLE(ImageRandomConstIteratorWithIndex ImageRandomConstIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageRandomConstIteratorWithIndex ITKIO   )

ADD_EXECUTABLE(NeighborhoodIterators1 NeighborhoodIterators1.cxx)
TARGET_LINK_LIBRARIES(NeighborhoodIterators1 ITKIO   )

ADD_EXECUTABLE(NeighborhoodIterators2 NeighborhoodIterators2.cxx)
TARGET_LINK_LIBRARIES(NeighborhoodIterators2 ITKIO   )

ADD_EXECUTABLE(NeighborhoodIterators3 NeighborhoodIterators3.cxx)
TARGET_LINK_LIBRARIES(NeighborhoodIterators3 ITKIO   )

ADD_EXECUTABLE(NeighborhoodIterators4 NeighborhoodIterators4.cxx)
TARGET_LINK_LIBRARIES(NeighborhoodIterators4 ITKIO   )

ADD_EXECUTABLE(NeighborhoodIterators5 NeighborhoodIterators5.cxx)
TARGET_LINK_LIBRARIES(NeighborhoodIterators5 ITKIO   )

ADD_EXECUTABLE(NeighborhoodIterators6 NeighborhoodIterators6.cxx)
TARGET_LINK_LIBRARIES(NeighborhoodIterators6 ITKAlgorithms ITKBasicFilters ITKIO )

IF(NOT BORLAND)
ADD_EXECUTABLE(ShapedNeighborhoodIterators1 ShapedNeighborhoodIterators1.cxx)
TARGET_LINK_LIBRARIES(ShapedNeighborhoodIterators1 ITKAlgorithms ITKBasicFilters ITKIO )

ADD_EXECUTABLE(ShapedNeighborhoodIterators2 ShapedNeighborhoodIterators2.cxx)
TARGET_LINK_LIBRARIES(ShapedNeighborhoodIterators2 ITKAlgorithms ITKBasicFilters ITKIO )
ENDIF(NOT BORLAND)

SET(ITERATORS_EXAMPLES ${CXX_TEST_PATH}/IteratorsExamples)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Iterators)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST(ImageLinearIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageLinearIteratorWithIndexTest.png
            ${TEMP}/ImageLinearIteratorWithIndexTest.png
  ImageLinearIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ImageLinearIteratorWithIndexTest.png
)

ADD_TEST(ImageRegionIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageRegionIteratorWithIndexTest.png
            ${TEMP}/ImageRegionIteratorWithIndexTest.png
  ImageRegionIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ImageRegionIteratorWithIndexTest.png
)

ADD_TEST(ImageRegionIteratorTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageRegionIteratorTest.png
            ${TEMP}/ImageRegionIteratorTest.png
  ImageRegionIteratorTest
        ${ITK_SOURCE_DIR}/Examples/Data/FatMRISlice.png
        ${TEMP}/ImageRegionIteratorTest.png
        20 70 210 140
)

ADD_TEST(ImageSliceIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageSliceIteratorWithIndexTest.mha
            ${TEMP}/ImageSliceIteratorWithIndexTest.mha
  ImageSliceIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensity3Slices.mha
        ${TEMP}/ImageSliceIteratorWithIndexTest.mha
        2
)


ADD_EXECUTABLE(IteratorsExamples IteratorsExamples.cxx )
TARGET_LINK_LIBRARIES(IteratorsExamples ITKIO ITKBasicFilters )
